import './Task.css';

function Task(props) {
  return (
    <div className="task">
      {props.name}
    </div>
  );
}

export default Task;