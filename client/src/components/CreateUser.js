import React, { useState } from "react";
import * as userActions from "../redux/actions/userActions";
import history from "../history";
import { connect } from "react-redux";
import { toast } from "react-toastify";

const CreateUser = ({ createUser }) => {
  const [username, setUsername] = useState("");

  const onUsernameChanged = event => {
    const { name, value } = event.target;
    setUsername(value);
  };

  const submitCreateUser = event => {
    event.preventDefault();
    if (username !== "") {
      createUser(username)
        .then(() => {
          history.push("/home");
        })
        .catch(error => {
          if (error.status === 409) {
            toast.error("Username already taken.");
          } else {
            throw error;
          }
        });
    }
  };

  return (
    <div className="newuser-container">
      <div class="username-prompt">Please, enter your username.</div>
      <form onSubmit={e => submitCreateUser(e)}>
        <input
          className="username-input"
          type="text"
          required
          onChange={onUsernameChanged}
        />
        <button class="newuser-button" type="submit">
          Create user
        </button>
      </form>
    </div>
  );
};

const mapDispatchToProps = {
  createUser: userActions.createUser
};

export default connect(null, mapDispatchToProps)(CreateUser);
