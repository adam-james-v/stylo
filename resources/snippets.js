function bulksave() {
    let state = document.getElementById("state-zone").innerHTML
    localStorage.setItem("state", state);
}

function bulkrestore() {
    let state = localStorage.getItem("state");
    document.getElementById("state-zone").innerHTML = state;
}

function reset() {
    localStorage.clear();
    location.reload();
}

function stateful() {
    let elems = document.getElementsByClassName("stateful");
    for (let elem of elems) {
        elem.contentEditable = true;
        elem.oninput = function () {bulksave();}
    }
}

function init() {
    stateful();
    if (localStorage.state) {
        bulkrestore();
        stateful();
    }
}
