import $ from "jquery";
import Sortable from "sortablejs";
import styles from "./app.module.less";
import { $feedback } from "./$feedbackContainer";
import { $div, $ul } from "./$elements";
import {
  $checkButton,
  $redoButton,
  $retryButton,
  $closeButton,
  $continueButton,
} from "./$buttons";
import { $staticLi, $sortableLi } from "./$listItems";
import { shuffle } from "./utilities";

const rootElement = document.getElementById("aneemo_sortableList");

const $root = $(rootElement);

const defaultSettings = {
  quizItems: [
    {
      question: "It likes to lick, plays with string and has a long tail",
      answer: "A cat named pickles",
    },
    {
      question: "Lives underwater and can be a bit scary",
      answer: "Sharky, of Sharky and George fame",
    },
    {
      question: "Barks sometimes and will east just about anything",
      answer: "A very large dalmation",
    },
    {
      question: "Likes trees and its faviourite food is banana",
      answer: "Your favourite primate, the capuchin monkey",
    },
  ],
  continueHandler: null,
  feedbackStrings: ["That was very good.", "Have a medal."],
};

const $app = (settings = defaultSettings) => {
  let { quizItems, continueHandler, feedbackStrings } = settings;

  // Unsortable items
  const staticItems = quizItems.map((item) =>
    $staticLi(item.question)
      .data("answer", item.answer)
      .addClass("sortQuiz_question")
  );

  // Sortable items
  const sortableItems = shuffle(quizItems).map((item) =>
    $sortableLi(item.answer).data("id", item.answer).addClass("sortQuiz_answer")
  );

  // Lists for the items
  const $staticList = $ul([styles.list, styles.staticList], staticItems);
  const $sortableList = $ul([styles.list, styles.sortableList], sortableItems);

  // Options for the sortable
  const sortableOptions = {
    dataIdAttr: "id",
    animation: 200,
  };

  // The sortable
  let sortable = new Sortable($sortableList[0], sortableOptions);

  // Buttons
  const $checkBtn = $checkButton(checkClickHandler);
  const $retryBtn = $retryButton(retryClickHandler);
  const $redoBtn = $redoButton(redoClickHandler);
  const $closeBtn = $closeButton(closeClickHandler);
  const $continueBtn = continueHandler && $continueButton(continueHandler);

  // Feedack
  const $feedbackContainer = $feedback(feedbackStrings, [
    $div(
      [styles.btnRow],
      [$closeBtn, $redoBtn, continueHandler && $continueBtn]
    ),
  ]);

  // Conainers
  const $listsContainer = $div(
    [styles.listsContainer],
    [$staticList, $sortableList]
  );
  const $btnContainer = $div([styles.btnContainer], [$retryBtn, $checkBtn]);
  const $container = $div(
    [styles.container],
    [$listsContainer, $btnContainer, $feedbackContainer]
  );

  // Event handlers

  // shorthand for correct/incorrect
  const clearFeedbackStyling = (selector) =>
    $("." + selector).removeClass(`${styles.correct} ${styles.incorrect}`);

  function retryClickHandler(e) {
    clearFeedbackStyling(styles.sortableItem);
    $retryBtn.prop("disabled", true);
    $checkBtn.prop("disabled", false);
    sortable.option("disabled", false);
  }

  function redoClickHandler(e) {
    clearFeedbackStyling(styles.sortableItem);
    $feedbackContainer.removeClass(styles.show);
    $retryBtn.prop("disabled", true);
    $checkBtn.prop("disabled", false);

    sortableItems.forEach(($element) => {
      $element.appendTo($sortableList);
    });
    sortable = new Sortable($sortableList[0], sortableOptions);
  }

  function checkClickHandler(e) {
    const $questions = $(".sortQuiz_question");
    const $answers = $(".sortQuiz_answer");
    // Clear styles; disable sorting and checking
    clearFeedbackStyling(styles.sortableItem);
    sortable.option("disabled", true);
    $checkBtn.prop("disabled", true);

    // The correct answers per question, in an array
    const correctAnswers = $questions
      .map((i, listElement) => $(listElement).data("answer"))
      .toArray();

    // The given answers, in an array
    const sortedAnswers = $answers.map((i, el) => $(el).data("id")).toArray();

    console.log("answers");
    console.log(correctAnswers);
    console.log("sortableItems");
    console.log(sortedAnswers);

    // The results, in an array
    const results = correctAnswers.map(
      (e, i) => correctAnswers[i] === sortedAnswers[i]
    );

    results.forEach((isCorrect, i) => {
      $answers.eq(i).addClass(isCorrect ? styles.correct : styles.incorrect);
    });

    const isAllCorrect = results.every((result) => result === true);

    if (isAllCorrect) {
      $feedbackContainer.addClass(styles.show);
    } else {
      $retryBtn.prop("disabled", false);
    }
  }

  function closeClickHandler() {
    $feedbackContainer.removeClass(styles.show);
  }

  return $container;
};

$root.append($app());
