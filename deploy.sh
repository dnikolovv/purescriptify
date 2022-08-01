#! /usr/bin/env bash
set -e

scriptdir="$(dirname "$0")"
cd "$scriptdir"

rm -rf ./dce-output
rm -rf ./dist
yarn run bundle
firebase deploy -P purescriptify
rm -rf ./dce-output
rm -rf ./dist
