console.log("Opening new-adventures DB…");
const request = window.indexedDB.open("new-adventures", 2);

request.onerror = (event) => {
    console.log("Got an error!", event);
};

request.onupgradeneeded = (event) => {
    console.log("Upgrading database…");
    const database = event.target.result;
    const objectStore = database.createObjectStore("feeds", { keyPath: "id" });
    objectStore.createIndex("name", "name", { unique: false });
};

request.onsuccess = (event) => {
    console.log("Opened database successfully");
    appLogic(event.target.result);
};

function appLogic(database) {
    console.log("Starting app with database:", database);
    document.querySelector(".add-feed").addEventListener("submit", (event) => {
        event.preventDefault();
        const url = event.target.querySelector("#feed-url").value;
        const name = event.target.querySelector("#feed-name").value;
        const id = parseInt(event.target.querySelector("#feed-id").value);
        const feed = {
            id,
            name,
            url,
        };
        database
            .transaction("feeds", "readwrite")
            .objectStore("feeds")
            .add(feed);
        showFeeds(database);
        console.log("Added feed to database:", feed);
    });
    showFeeds(database);
}

function showFeeds(database) {
    const feedTableBody = document.querySelector(".feeds tbody");
    let rows = [];
    database.transaction("feeds").objectStore("feeds").openCursor().onsuccess =
        (event) => {
            const cursor = event.target.result;
            if (cursor != undefined) {
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
                    feedTableBody.replaceChildren(...rows);
                }
            }
        };
}
