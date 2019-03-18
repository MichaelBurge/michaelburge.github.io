Array.prototype.forEach.call(document.getElementsByClassName("spoiler"), element => {
    let pre = element.children[0];
    let btn = document.createElement("button");
    var f_show = () => {
        pre.style.display = "block";
        btn.onclick = f_hide;
        btn.innerText = "Hide";
    };
    var f_hide = () => {
        pre.style.display = "none";
        btn.onclick = f_show;
        btn.innerText = "Show";
    };
    btn.onclick = f_show;
    f_hide();
    element.insertBefore(btn, pre);
});
