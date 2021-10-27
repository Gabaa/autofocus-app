import Task from "./Task";
import './TaskList.css';

function TaskList() {
  const tasks = ["Brush teeth", "Clean kitchen", "Do dishes"];

  return (
    <div className="task-list">
      <h2>Task list</h2>
      <div className="tasks">
        {tasks.map(task => {
          return <Task name={task} />
        })}
      </div>
    </div>
  );
}

export default TaskList;