const express = require("express");
const app = express();

app.listen(3000, () => {
  console.log("Server running on port 3000");
});

// Endpoint that calculates the probability of hospitalization, ICU and deaths given user input.
app.get("/cv19-risk-score", (req, res) => {
  // sex and age are mandatory. Only conditions is optional.
  const sex = req.query.sex;
  const age = req.query.age;
  let conditions = "NULL";
  if (typeof req.query.conditions === "object") {
    conditions = "list(";
    req.query.conditions.forEach((element) => {
      conditions += '"' + element + '",';
    });
    conditions = conditions.replace(/,+$/, "") + ")";
  }

  const { exec } = require("child_process");
  // Execute R script.
  exec(
    "Rscript ../src/susceptibility_wrapper.R " +
      age +
      " " +
      sex +
      " '" +
      conditions +
      "'",
    function (err, stdout, stderr) {
      if (err) {
        // node couldn't execute the command
        res.json(err);
        return;
      }

      res.json(JSON.parse(stdout));
    }
  );
});
