// Blank element
import $ from "jquery";

const $element = (type, classes = [], children = null) => {
  const classListStr = classes.length > 0 ? `class="${classes.join(" ")}"` : "";

  const $el = $(`<${type} ${classListStr}></${type}>`);

  if (children) {
    $el.append(children);
  }

  return $el;
};

// HTML elements
export const $div = (classes = [], children = null) =>
  $element("div", classes, children);

export const $p = (classes = [], text = "") => $element("p", classes, [text]);

export const $ul = (classes = [], children = null) =>
  $element("ul", classes, children);

export const $li = (classes = [], children = null) =>
  $element("li", classes, children);

export const $i = (classes = [], children = null) =>
  $element("i", classes, children);

export const $btn = (classes = [], children = [], onclick = (e) => {}) => {
  const $btn = $element("button", classes, children);
  $btn.on("click", onclick);
  return $btn;
};
