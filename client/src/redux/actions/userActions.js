import * as ActionTypes from "./actionTypes";
import * as usersApi from "../../api/usersApi";

export function loadCurrentUserSuccess(user) {
  return { type: ActionTypes.LOAD_CURRENT_USER_SUCCESS, user };
}

export function loadCurrentUserFailure() {
  return { type: ActionTypes.LOAD_CURRENT_USER_FAILURE };
}

export function createUser(username) {
  return function(dispatch) {
    return usersApi
      .createUser(username)
      .then(response => {
        localStorage.setItem("access_token", response.token);
        dispatch(loadCurrentUser());
      })
      .catch(error => {
        throw error;
      });
  };
}

export function loadUsersSuccess(users) {
  return { type: ActionTypes.LOAD_USERS_SUCCESS, availableUsers: users };
}

export function loadUsers() {
  return function(dispatch) {
    return usersApi.getAvailableUsers().then(response => {
      dispatch(loadUsersSuccess(response));
    });
  };
}

export function loadCurrentUser() {
  return function(dispatch) {
    return usersApi
      .getCurrentUser()
      .then(user => {
        dispatch(loadCurrentUserSuccess(user));
      })
      .catch(() => {
        dispatch(loadCurrentUserFailure());
      });
  };
}
