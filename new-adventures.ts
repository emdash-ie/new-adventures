console.log("Opening new-adventures DB…");
const request: IDBOpenDBRequest = window.indexedDB.open("new-adventures", 2);

request.onerror = (event) => {
  console.log("Got an error!", event);
};

request.onupgradeneeded = (_event: IDBVersionChangeEvent) => {
  console.log("Upgrading database…");
  const database = request.result;
  const objectStore = database.createObjectStore("feeds", { keyPath: "id" });
  objectStore.createIndex("name", "name", { unique: false });
};

request.onsuccess = (_event) => {
  console.log("Opened database successfully");
  appLogic(request.result);
};

function appLogic(database: IDBDatabase) {
  console.log("Starting app with database:", database);
  document.querySelector(".add-feed")?.addEventListener("submit", (event) => {
    event.preventDefault();
    if (event.target != null) {
      const form: HTMLFormElement = event.target as HTMLFormElement;
      const url = (form.querySelector("#feed-url") as HTMLInputElement).value;
      const name = (form.querySelector("#feed-name") as HTMLInputElement).value;
      const id = parseInt(
        (form.querySelector("#feed-id") as HTMLInputElement).value,
      );
      const feed = {
        id,
        name,
        url,
      };
      database.transaction("feeds", "readwrite").objectStore("feeds").add(feed);
      showFeeds(database);
      console.log("Added feed to database:", feed);
    }
  });
  showFeeds(database);
}

function showFeeds(database: IDBDatabase) {
  const feedTableBody = document.querySelector(".feeds tbody");
  let rows: HTMLTableRowElement[] = [];
  database.transaction("feeds").objectStore("feeds").openCursor().onsuccess = (
    event,
  ) => {
    const cursor = (event.target as IDBRequest)
      ?.result as IDBCursorWithValue | null;
    if (cursor != null) {
      let row = document.createElement("tr");
      for (let field of ["name", "url", "id"]) {
        const element = document.createElement("td");
        element.append(cursor.value[field]);
        row.appendChild(element);
      }
      rows.push(row);
      cursor.continue();
    } else {
      if (rows.length > 0) {
        feedTableBody?.replaceChildren(...rows);
      }
    }
  };
}
