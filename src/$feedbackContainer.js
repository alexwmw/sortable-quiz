import { $div, $i, $p } from "./$elements";

export const $feedback = (children = []) => {
  const correctIcon = $i(["fas", "fa-check", "feedbackIcon", "correctIcon"]);
  const thatsRight = $p(["feedback_text"], "That's right!");

  children = [correctIcon, thatsRight, ...children];

  return $div(["feedback"], children);
};
