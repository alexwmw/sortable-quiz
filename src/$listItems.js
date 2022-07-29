import { $li, $i } from "./$elements";

export const $sortableLi = (text = "") => {
  const sortableIcon = $i(["fas", "fa-sort"]);
  const correctIcon = $i(["fas", "fa-check", "feedbackIcon", "correctIcon"]);
  const incorrectIcon = $i([
    "fas",
    "fa-times",
    "feedbackIcon",
    "incorrectIcon",
  ]);
  const $el = $li(
    ["itemStyle", "sortableStyle", "sortableItem"],
    [sortableIcon, text, correctIcon, incorrectIcon]
  );
  return $el;
};

export const $staticLi = (text = "") => {
  return $li(["itemStyle", "staticStyle"], text);
};
