import { $btn } from "./$elements";

export const $checkButton = (children = [], onclick = (e) => {}) =>
  $btn(["btn checkBtn"], children, onclick);

export const $resetButton = (children = [], onclick = (e) => {}) =>
  $btn(["btn resetBtn"], children, onclick).prop("disabled", true);

export const $redoButton = (children = [], onclick = (e) => {}) =>
  $btn(["btn redoBtn"], children, onclick);
