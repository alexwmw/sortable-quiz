import { $btn } from "./$elements";
import styles from "./app.module.less";

export const $checkButton = (onclick = (e) => {}) => {
  const title = "<span>Check</span>";
  return $btn([styles.btn, styles.checkBtn], [title], onclick);
};

export const $retryButton = (onclick = (e) => {}) => {
  const title = "</span>Try again</span>";
  return $btn([styles.btn, styles.resetBtn], [title], onclick).prop(
    "disabled",
    true
  );
};

export const $redoButton = (onclick = (e) => {}) => {
  const title = "</span>Reset</span>";
  return $btn([styles.btn, styles.redoBtn], [title], onclick);
};

export const $closeButton = (onclick = (e) => {}) => {
  const title = "</span>Close</span>";
  return $btn([styles.btn, styles.closeBtn], [title], onclick);
};

export const $continueButton = (onclick = (e) => {}) => {
  const title = "</span>Complete and continue</span>";
  return $btn([styles.btn, styles.continueBtn], [title], onclick);
};
