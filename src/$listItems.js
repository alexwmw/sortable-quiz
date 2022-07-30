import { $li, $i } from "./$elements";
import styles from "./app.module.less";

export const $sortableLi = (text = "") => {
  const sortableIcon = $i(["fa", "fas", "fa-sort", styles.sortIcon]);
  const correctIcon = $i([
    "fa",
    "fas",
    "fa-check",
    styles.feedbackIcon,
    styles.correctIcon,
  ]);
  const incorrectIcon = $i([
    "fa",
    "fas",
    "fa-times",
    styles.feedbackIcon,
    styles.incorrectIcon,
  ]);
  const $el = $li(
    [styles.itemStyle, styles.sortableItem],
    [text, sortableIcon, correctIcon, incorrectIcon]
  );
  return $el;
};

export const $staticLi = (text = "") => {
  return $li([styles.itemStyle, styles.staticItem], text);
};
