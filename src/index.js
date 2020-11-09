import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.saveTodos.subscribe(function(todos) {
	localStorage.setItem('todos-elm', JSON.stringify(todos))
});

app.ports.todoReader.send(JSON.parse(localStorage.getItem('todos-elm')));

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
