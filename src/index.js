import "./main.css";
import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

var app = Elm.Main.init({
  node: document.getElementById("root")
});

app.ports.portSetLocalStorage.subscribe(req => {
  console.log("TESTだよ");
  console.log(req[1]);
  var data = convertThreads(req[1]);
  console.log(data);
  app.ports.syncElm.send(data);
});

function convertThreads(threads) {
  var result = new Array();
  threads.forEach(element => {
    result.push(convert(element));
  });

  return result;
}

function convert(thread) {
  return {
    title: thread.title,
    owner: thread.owner,
    comments: thread.comments
  };
}
registerServiceWorker();
