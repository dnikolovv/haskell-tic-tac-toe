import React from "react";
import * as invitationsApi from "../api/invitationsApi";

const InvitedToGameNotification = ({ invitationId, closeToast }) => {
  const accept = () => {
    invitationsApi.accept(invitationId);
    closeToast();
  };

  const decline = () => {
    invitationsApi.decline(invitationId);
    closeToast();
  };

  return (
    <div className="notification-container">
      <p>You've been invited to a game!</p>
      <button className="accept-invitation-button" onClick={() => accept()}>
        Accept
      </button>
      <button className="decline-invitation-button" onClick={() => decline()}>
        Decline
      </button>
    </div>
  );
};

export default InvitedToGameNotification;
