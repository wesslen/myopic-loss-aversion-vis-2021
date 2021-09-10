import React from "react";
import { useHistory, useLocation } from "react-router-dom";
import BottomNavigation from "@material-ui/core/BottomNavigation";
import BottomNavigationAction from "@material-ui/core/BottomNavigationAction";
import { makeStyles } from "@material-ui/core/styles";

const useStyles = makeStyles((theme) => ({
  root: {
    // width: "100%",
    backgroundColor: "#d7d7d7",
    // pointerEvents: "None",
    // height: 48,
    // padding: '0 30px',
    color: 'white',
    "&$selected": {
      color: "white",
    },
  },
  selected: {},
}));

// const useStyles = makeStyles({
//   bottomNav: {
//     width: "100%",
//     height: "7%",
//     backgroundColor: "orange",
//     // position: "fixed",
//     // bottom: 0,
//   },
// });

const BottomNav = (props) => {
  const classes = useStyles();
  const history = useHistory();
  let location = useLocation();
  //   console.log(history);
  const setHistory = (newValue) => {
    history.push(newValue);
  };

  console.log(location.pathname);
  return (
    <BottomNavigation
      value={location.pathname}
      // onChange={(event, newValue) => {
      //   console.log(newValue);
      //   setPageIndex(newValue);
      //   setHistory(newValue);
      // }}
      showLabels
      className={classes.root}
      style={{ height: props.height }}
    >
      <BottomNavigationAction
        label="Consent"
        value="/consent"
        className={classes.selected}
      />
      <BottomNavigationAction
        label="Pre-questionaire"
        value="/pre"
        className={classes.selected}
      />
      <BottomNavigationAction
        label="Instructions 1"
        value="/instructions1" // need to pass instructions1 - instructions8.js
        className={classes.selected}
      />

      <BottomNavigationAction
        label="Round 1"
        value="/task1"
        className={classes.selected}
      />
      <BottomNavigationAction
        label="Instructions 2"
        value="/instructionsTask2"
        className={classes.selected}
      />
      <BottomNavigationAction
        label="Round 2"
        value="/task2"
        className={classes.selected}
      />
      {/*<BottomNavigationAction*/}
      {/*  label="Instructions 3"*/}
      {/*  value="/instructions3"*/}
      {/*  className={classes.selected}*/}
      {/*/>*/}
      {/*<BottomNavigationAction*/}
      {/*  label="Round 3"*/}
      {/*  value="/task3"*/}
      {/*  className={classes.selected}*/}
      {/*/>*/}
      <BottomNavigationAction
        label="Post"
        value="/post"
        className={classes.selected}
      />
      <BottomNavigationAction
        label="Debrief"
        value="/debrief"
        className={classes.selected}
      />
    </BottomNavigation>
  );
};

export default BottomNav;
