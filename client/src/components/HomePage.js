import React, { useEffect } from "react";
import PropTypes from "prop-types";
import { connect } from "react-redux";
import * as userActions from "../redux/actions/userActions";
import * as usersApi from "../api/usersApi";

const HomePage = ({ currentUser, availableUsers, loadAvailableUsers }) => {
  useEffect(() => {
    loadAvailableUsers();
  }, []);

  const inviteUser = userId => {
    usersApi.inviteUser(userId);
  };

  const usersToShow = availableUsers.filter(user => user.id != currentUser.id);

  return (
    <div className="players-container">
      <h1>Available players:</h1>
      <ul className="players-list">
        <li className="player" key={currentUser.id}>
          {currentUser.name} (You)
        </li>
        {usersToShow.map(user => {
          return (
            <li className="player" key={user.id}>
              {user.name}{" "}
              <button
                className="invite-button"
                onClick={() => inviteUser(user.id)}
              >
                Invite
              </button>
            </li>
          );
        })}
      </ul>
    </div>
  );
};

function mapStateToProps(state) {
  return {
    currentUser: state.currentUser,
    availableUsers: state.availableUsers
  };
}

const mapDispatchToProps = {
  loadAvailableUsers: userActions.loadUsers
};

HomePage.propTypes = {
  availableUsers: PropTypes.array.isRequired
};

export default connect(mapStateToProps, mapDispatchToProps)(HomePage);
