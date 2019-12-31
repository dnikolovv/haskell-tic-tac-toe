import { joinUrlWithRoute } from "../utils/urlUtils";

const baseUrl = process.env.REACT_APP_SOCKETS_URL;

const sockets = [];

export function connectTo(socketRoute, callback) {
  const token = localStorage.getItem("access_token");
  const url = joinUrlWithRoute(baseUrl, socketRoute + `?access_token=${token}`);
  const socket = new WebSocket(url);
  socket.onmessage = callback;
  socket.onopen = onOpenHandler;
  socket.onclose = onCloseHandler;
  socket.onerror = onErrorHandler;
  sockets.push(socket);
}

export function closeAllOpenConnections() {
  sockets.forEach(socket => {
    socket.close();
  });
}

const onOpenHandler = event => {
  console.log("OPEN");
  console.log(event);
};

// TODO: Remove from active sockets collection
const onCloseHandler = event => {
  console.log("CLOSED!");
  console.log(event);
};

const onErrorHandler = event => {
  console.log("ERROR!");
  console.log(event);
};
