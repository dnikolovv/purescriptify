import { initializeApp } from "firebase/app";
import { getAnalytics } from "firebase/analytics";

const firebaseConfig = {
  apiKey: "AIzaSyClKMpxtbTKkrJDo7RmUV8Ucm6KLfDzoCo",
  authDomain: "purescriptify.firebaseapp.com",
  projectId: "purescriptify",
  storageBucket: "purescriptify.appspot.com",
  messagingSenderId: "475139771273",
  appId: "1:475139771273:web:423e214baf655eede76f58",
  measurementId: "G-NVC093SV8B"
};

const app = initializeApp(firebaseConfig);
getAnalytics(app);

require('@fontsource/inter');
require("../output/Main/index.js").main();
