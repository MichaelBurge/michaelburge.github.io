Array.prototype.forEach.call(document.getElementsByClassName("spoiler"), element => {
    let pre = element.children[0];
    pre.style.visibility = "hidden";
    btn = document.createElement("button"); btn.innerText = "Show";
    btn.onclick = () => {
        pre.style.visibility = "visible";
    };
    element.insertBefore(btn, pre);
});
