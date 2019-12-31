import * as wsClient from "../api/webSocketClient";
import React from "react";
import * as ActionTypes from "../redux/actions/actionTypes";
import { toast } from "react-toastify";
import InvitedToGameNotification from "../components/Notifications";
import history from "../history";

const notificationsMiddleware = store => next => action => {
  if (action.type === ActionTypes.LOAD_CURRENT_USER_SUCCESS) {
    wsClient.connectTo("/notifications/subscribe", handleNotificationReceived);
  }

  next(action);
};

const handleNotificationReceived = notificationEvent => {
  const notification = JSON.parse(notificationEvent.data);
  switch (notification.type) {
    case "InvitedToGame":
      toast(<InvitedToGameNotification invitationId={notification.id} />);
      break;
    case "GameStarted":
      toast.success("A game has started!");
      const gameId = notification.payload;
      history.push("/game/" + gameId);
      break;
    case "InvitationDeclined":
      toast.error("Your invitation has been declined!");
      break;
    default:
      break;
  }
};

export default notificationsMiddleware;
