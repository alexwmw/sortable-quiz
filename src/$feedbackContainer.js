import { $div, $i, $p } from "./$elements";
import styles from "./sortQuiz.module.less";

export const $feedback = (feedbackStrings = [], children = []) => {
  const $correctIcon = $i([
    "fa",
    "fas",
    "fa-check-square",
    styles.feedbackIcon,
    styles.correctIcon,
  ]);
  const $affirmation = $p([styles.feedbackTitle], "Well done!");

  const $pArray = feedbackStrings.map((string) => {
    return $p([styles.feedbackText], string);
  });

  children = [$correctIcon, $affirmation, ...$pArray, ...children];

  return $div([styles.feedback], children);
};
