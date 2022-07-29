export const shuffle = (array, original = array) => {
  let currentIndex = array.length,
    randomIndex;
  let newArray = [...array];

  // While there remain elements to shuffle.
  while (currentIndex != 0) {
    // Pick a remaining element.
    randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex--;

    // And swap it with the current element.
    [newArray[currentIndex], newArray[randomIndex]] = [
      newArray[randomIndex],
      newArray[currentIndex],
    ];
  }
  // If item is in original place, shuffle again
  if (newArray.some((element, index) => original[index] === newArray[index])) {
    return shuffle(newArray, original);
  } else {
    return newArray;
  }
};
