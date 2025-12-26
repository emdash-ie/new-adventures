import type { OrgFile, OrgTree, OrgTodo, Todos } from "./org-to-do";

declare function getTodos(onSuccess: (todos: Todos) => void, onError: (e: Error) => void): void;

console.log("Opening new-adventures DB…");
const request: IDBOpenDBRequest = window.indexedDB.open("new-adventures", 4);

request.onerror = (event) => {
  console.log("Got an error!", event);
};

request.onupgradeneeded = (_event: IDBVersionChangeEvent) => {
  console.log("Upgrading database…");
  const database = request.result;
  try {
    database.deleteObjectStore("todos");
  } catch (e) {
    console.log("Error deleting todos object store, ignoring");
  }
  const objectStore = database.createObjectStore("todos");
};

request.onsuccess = (_event) => {
  console.log("Opened database successfully");
  appLogic(request.result);
};

function appLogic(database: IDBDatabase) {
  console.log("Starting app with database:", database);
  getTodos(
    todos => {
      console.log("Fetched todos");
      database.transaction("todos", "readwrite").objectStore("todos").add(todos, 1);
      displayToDos(database);
    },
    error => console.log("Error fetching to-dos:", error)
  )
  document.querySelector(".add-todo")?.addEventListener("submit", (event) => {
    event.preventDefault();
    if (event.target != null) {
      const form: HTMLFormElement = event.target as HTMLFormElement;
      const heading = (form.querySelector("#todo-heading") as HTMLInputElement).value;
      const state: OrgTodo | "" = (form.querySelector("#todo-state") as HTMLSelectElement).value as (OrgTodo | "");
      const preamble = (form.querySelector("#todo-preamble") as HTMLTextAreaElement).value;

      const todo: OrgTree = {
        heading,
        todoState: state === "" ? null : state,
        preamble,
        children: [] as OrgTree[]
      };
      database.transaction("todos", "readwrite").objectStore("todos").add(todo);
      displayToDos(database);
      console.log("Added todo to database:", todo);
    }
  });
  displayToDos(database);
}

function displayToDos(database: IDBDatabase) {
  const feedTableBody = document.querySelector(".todos");
  let todos: HTMLElement[] = [];
  database.transaction("todos").objectStore("todos").openCursor().onsuccess = (
    event,
  ) => {
    const cursor = (event.target as IDBRequest)
      ?.result as IDBCursorWithValue | null;
    if (cursor != null) {
      todos.push(showTodos(cursor.value))
      cursor.continue();
    } else {
      if (todos.length > 0) {
        feedTableBody?.replaceChildren(...todos);
      }
    }
  };
}

function showTree(tree: OrgTree): HTMLDetailsElement {
  let details = document.createElement("details");
  let summary = document.createElement("summary");
  summary.replaceChildren(`${tree.todoState ?? ""} ${tree.heading}`);
  details.replaceChildren(
    summary,
    `
\n${tree.preamble}`,
    ...tree.children.map(t => showTree(t))
  );
  return details;
}

function showTodos(todos: Todos): HTMLDivElement {
  let div = document.createElement("div");
  for (let filename of Object.keys(todos)) {
    let p = document.createElement("p");
    p.replaceChildren(`Org file at ${filename} with title ${todos[filename]?.orgMeta.title}`);
    div.appendChild(p);
  }
  return div;
}
