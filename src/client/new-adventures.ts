import type { OrgFile, OrgTree, OrgTodo } from "./org-to-do";

console.log("Opening new-adventures DB…");
const request: IDBOpenDBRequest = window.indexedDB.open("new-adventures", 3);

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
  const objectStore = database.createObjectStore("todos", { keyPath: "heading" });
};

request.onsuccess = (_event) => {
  console.log("Opened database successfully");
  appLogic(request.result);
};

function appLogic(database: IDBDatabase) {
  console.log("Starting app with database:", database);
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
      todos.push(showTree(cursor.value))
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
