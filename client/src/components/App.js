import React from "react";
import PageNotFound from "./PageNotFound";
import HomePage from "./HomePage";
import CreateUser from "./CreateUser";
import Game from "./Game";
import UserProvider from "./UserProvider";
import { Route, Switch } from "react-router-dom";
import { ToastContainer } from "react-toastify";
import "react-toastify/dist/ReactToastify.css";

const App = () => {
  return (
    <div className="container-fluid">
      <Switch>
        <Route exact path="/" component={CreateUser} />
        <Route exact path="/home" component={HomePage} />
        <Route path="/game/:gameId" component={Game} />
        <Route component={PageNotFound} />
      </Switch>
      <UserProvider />
      <ToastContainer autoClose={3000} hideProgressBar={true} />
    </div>
  );
};

export default App;
