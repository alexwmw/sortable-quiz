import $ from "jquery";
import Sortable from "sortablejs";
import "./app.less";
import { $div, $ul } from "./$elements";
import { $checkButton, $redoButton, $resetButton } from "./$buttons";
import { $staticLi, $sortableLi } from "./$listItems";
import { shuffle } from "./utilities";
import { $feedback } from "./$feedbackContainer";

const rootElement = document.getElementById("aneemo_sortableList");

const $root = $(rootElement);

const $app = (settings = { quizItems: [] }) => {
  let { quizItems } = settings;

  if (quizItems)
    quizItems = [
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
    ];
  // Unsortable items
  const staticItems = quizItems.map((item) =>
    $staticLi(item.question).data("answer", item.answer)
  );

  // Sortable items
  const sortableItems = shuffle(quizItems).map((item) =>
    $sortableLi(item.answer).data("id", item.answer)
  );

  // Lists for the items
  const $staticList = $ul(["list", "staticList"], staticItems);
  const $sortableList = $ul(["list", "sortableList"], sortableItems);

  // Options for the sortable
  const sortableOptions = {
    dataIdAttr: "id",
    animation: 200,
  };

  // The sortable
  let sortable = new Sortable($sortableList[0], sortableOptions);

  // Buttons
  const $checkBtn = $checkButton(["<span>Check</span>"], checkClickHandler);
  const $resetBtn = $resetButton(
    ["</span>Try again</span>"],
    resetClickHandler
  );
  const $redoBtn = $redoButton(["</span>Reset</span>"], redoClickHandler);

  // Feedack
  const $feedbackContainer = $feedback([$redoBtn.prop("disabled", false)]);

  // Conainers
  const $listsContainer = $div(
    ["listsContainer"],
    [$staticList, $sortableList]
  );
  const $btnContainer = $div(["btnContainer"], [$resetBtn, $checkBtn]);
  const $container = $div(
    ["container"],
    [$listsContainer, $btnContainer, $feedbackContainer]
  );

  // Event handlers

  function resetClickHandler(e) {
    $(".sortableItem").removeClass("correct incorrect");
    $resetBtn.prop("disabled", true);
    $checkBtn.prop("disabled", false);

    sortableItems.forEach(($element) => {
      $element.appendTo($sortableList);
    });
    sortable = new Sortable($sortableList[0], sortableOptions);
  }

  function redoClickHandler(e) {
    $(".sortableItem").removeClass("correct incorrect");
    $resetBtn.prop("disabled", true);
    $checkBtn.prop("disabled", false);

    sortableItems.forEach(($element) => {
      $element.appendTo($sortableList);
    });
    sortable = new Sortable($sortableList[0], sortableOptions);
  }

  function checkClickHandler(e) {
    sortable.option("disabled", true);
    $checkBtn.prop("disabled", true);

    $(".sortableItem").removeClass("correct incorrect");

    const answers = $(".staticList > li")
      .map((i, el) => $(el).data("answer"))
      .toArray();
    const sortedItems = $(".sortableList > li")
      .map((i, el) => $(el).data("id"))
      .toArray();

    let correct = true;

    answers.forEach((e, i, a) => {
      const thisIsCorrect = answers[i] === sortedItems[i];
      correct = correct && thisIsCorrect;

      $(".sortableList > li")
        .eq(i)
        .addClass(thisIsCorrect ? "correct" : "incorrect");
    });

    if (!correct) {
      $resetBtn.prop("disabled", false);
    }
  }

  return $container;
};

$root.append($app());
