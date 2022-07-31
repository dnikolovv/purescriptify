{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Purescriptify.API.Definition where

import Effectful (Eff)
import qualified Effectful as E
import qualified Effectful.Error.Static as ES
import Purescriptify.API.Auth (APIUser)
import Purescriptify.API.DomainError (DomainError)
import qualified Purescriptify.API.DomainError as DomainError
import Purescriptify.API.ManageUsers (ManageUsers, deleteUser, getAllUsers, getUser, newUser, updateUser)
import Purescriptify.API.Types
import RIO
import Servant
import Servant.Auth.Server

type UsersAPI =
  PublicAPI
    :<|> ProtectedAPI

type PublicAPI =
  "users" :> Get '[JSON] [User]
    :<|> ( "user"
             :> ( Capture "userId" UserId :> Get '[JSON] User
                    :<|> ReqBody '[JSON] CreateUserRequest :> Post '[JSON] User
                    :<|> Capture "userId" UserId :> ReqBody '[JSON] UpdateUserRequest :> Put '[JSON] ()
                )
         )

type ProtectedAPI =
  Auth '[JWT] APIUser
    :> ("user" :> Capture "userId" UserId :> Delete '[JSON] ())

type UsersTable = IORef (HashMap UserId UserData)

server ::
  ManageUsers E.:> es =>
  ES.Error DomainError E.:> es =>
  ServerT UsersAPI (Eff es)
server =
  publicServer
    :<|> protectedServer
  where
    publicServer =
      getAllUsers
        :<|> getUser
        :<|> (\CreateUserRequest {..} -> newUser email username)
        :<|> (\uId UpdateUserRequest {..} -> updateUser uId newEmail newUsername)

    protectedServer (Authenticated _) uId = do
      deleteUser uId
    protectedServer _ _ =
      ES.throwError DomainError.Unauthorized