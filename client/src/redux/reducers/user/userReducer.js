import * as ActionTypes from "../../actions/actionTypes";
import initialState from "../../initialState";

export function userReducer(state = initialState, action) {
  switch (action.type) {
    case ActionTypes.LOAD_USERS_SUCCESS:
      return { ...state, availableUsers: action.availableUsers };
    case ActionTypes.LOAD_CURRENT_USER_SUCCESS:
      return { ...state, currentUser: action.user };
    default:
      return state;
  }
}
