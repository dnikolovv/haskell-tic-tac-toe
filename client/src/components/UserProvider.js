import React, { useEffect } from "react";
import * as userActions from "../redux/actions/userActions";
import { connect } from "react-redux";
import PropTypes from "prop-types";

const UserProvider = ({ loadCurrentUser }) => {
  useEffect(() => {
    if (tokenIsSet()) {
      loadCurrentUser();
    }
  }, []);

  return <div />;
};

UserProvider.propTypes = {
  loadCurrentUser: PropTypes.func.isRequired
};

const mapDispatchToProps = {
  loadCurrentUser: userActions.loadCurrentUser
};

function tokenIsSet() {
  return localStorage.getItem("access_token") !== null;
}

export default connect(
  null,
  mapDispatchToProps
)(UserProvider);
