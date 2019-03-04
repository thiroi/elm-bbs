import "./main.css";
import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

console.log(localStorage.getItem("threads"));

var app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: JSON.parse(localStorage.getItem("threads"))
});

app.ports.portSetLocalStorage.subscribe(req => {
  const newItem = req[1];
  localStorage.setItem("threads", JSON.stringify(newItem));
  app.ports.syncElm.send(newItem);
});

// function convertThreads(threads) {
//   var result = new Array();
//   threads.forEach(element => {
//     result.push(convert(element));
//   });

//   return result;
// }

// function convert(thread) {
//   return {
//     title: thread.title,
//     owner: thread.owner,
//     comments: thread.comments
//   };
// }
registerServiceWorker();
