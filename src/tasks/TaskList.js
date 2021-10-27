import { List } from "immutable";
import _ from "lodash";
import { useState } from "react";
import Task from "./Task";
import './TaskList.css';

function TaskList() {
  const TASKS_PER_PAGE = 20;
  const [tasks, setTasks] = useState(List(["Brush teeth", "Clean kitchen", "Do dishes"]));

  return (
    <div className="task-list">
      <h2>Task list</h2>
      <div className="tasks">
        {_.range(TASKS_PER_PAGE).map(i => {
          if (i < tasks.size) {
            return <Task key={i} name={tasks.get(i)} />
          }
          return <Task key={i} name="???" />
        })}
      </div>
    </div>
  );
}

export default TaskList;