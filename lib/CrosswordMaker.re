type direction =
  | Down
  | Right;


type contents =
  | Char(char)
  | Empty
  | FilledIn;

let contents_equal = (x: contents, y: contents) =>
  switch (x, y) {
  | (Char(c), Char(d)) when c == d => true
  | (Empty, Empty) => true
  | (FilledIn, FilledIn) => true
  | _ => false
  };

type row = array(contents);

type grid = array(row);

type partialWord = {
  dir: direction,
  col: int,
  row: int,
  start: int,
  crossIndex: int,
  length: int,
};

type completedWord = {
  dir: direction,
  col: int,
  row: int,
  start: int,
  crossIndex: int,
  length: int,
  value: string,
};

let getRandomWord = (size: int) : partialWord => {
  let length = Random.int(size - 2) + 3;
  let start = Random.int(size - length);
  let crossIndex = Random.int(size);
  Random.bool() ?
    {
      dir: Down,
      col: crossIndex,
      row: start,
      start,
      crossIndex,
      length,
    } :
    {
      dir: Right,
      col: start,
      row: crossIndex,
      start,
      crossIndex,
      length,
    };
};

let intersects = (wordA: partialWord, wordB: partialWord) : bool =>
  wordA.dir === wordB.dir ?
    true :
    wordA.start <= wordB.crossIndex
    && wordA.start
    + wordA.length
    - 1 >= wordB.crossIndex
    && wordB.start <= wordA.crossIndex
    && wordB.start
    + wordB.length
    - 1 >= wordA.crossIndex;

let checkWord = (word: partialWord, words: list(partialWord)) : bool => {
  let intersectsWord = intersects(word);
  List.length(words) > 0 && ! List.exists(intersectsWord, words) ?
    false :
    !
      List.exists(
        (thisWord: partialWord) =>
          thisWord.dir === word.dir ?
            (
              thisWord.start
              + thisWord.length
              - 1 >= word.start
              || word.start
              + word.length
              - 1 >= thisWord.start
            )
            && thisWord.crossIndex
            - word.crossIndex <= 1
            && thisWord.crossIndex
            - word.crossIndex >= (-1) :
            (
              thisWord.start
              - 1 === word.crossIndex
              || thisWord.start
              + thisWord.length === word.crossIndex
            )
            && thisWord.crossIndex >= word.start
            && thisWord.crossIndex <= word.start
            + word.length
            - 1
            || (
              word.start
              - 1 === thisWord.crossIndex
              || word.start
              + word.length === thisWord.crossIndex
            )
            && word.crossIndex >= thisWord.start
            && word.crossIndex <= thisWord.start
            + thisWord.length
            - 1,
        words,
      );
};

let updateGrid = (word: partialWord, _grid: grid) : grid => {
  for (ind in 0 to word.length - 1) {
    if (word.dir === Right) {
      _grid[word.crossIndex][word.start + ind] = Empty;
    } else {
      _grid[word.start + ind][word.crossIndex] = Empty;
    };
  };
  _grid;
};

let makeGrid = (size: int) => {
  let grid = ref(Array.make_matrix(size, size, FilledIn));
  let target = size;
  let count = ref(0);
  let words: ref(list(partialWord)) = ref([]);
  while (List.length(words^) < target) {
    count := count^ + 1;
    let thisWord = getRandomWord(size);
    if (checkWord(thisWord, words^)) {
      words := [thisWord, ...words^];
      grid := updateGrid(thisWord, grid^);
    };
  };
};

type letterConstraint = {
  letter: int,
  value: char,
};

let calcConstraints =
    (wordShape: partialWord, fittedWords: list(completedWord))
    : list(letterConstraint) =>
  fittedWords
  |> List.fold_left(
       (letterConstraints, fittedWord) => (
         fittedWord.dir === wordShape.dir
         || fittedWord.crossIndex < wordShape.start
         || fittedWord.crossIndex > wordShape.start
         + wordShape.length
         - 1
         || wordShape.crossIndex
         - fittedWord.start < 0
         || wordShape.crossIndex
         - fittedWord.start >= String.length(fittedWord.value) ?
           letterConstraints :
           {
             let value =
               wordShape.crossIndex
               - fittedWord.start
               |> String.get(fittedWord.value);
             [
               {letter: fittedWord.crossIndex - wordShape.start, value},
               ...letterConstraints,
             ];
           }:
           list(letterConstraint)
       ),
       [],
     );

let fitsConstraints =
    (
      _constraints: list(letterConstraint),
      reject: list(string),
      word: string,
    ) =>
  ! List.exists(rejectWord => rejectWord === word, reject)
  && !
       List.exists(
         _constraint => word.[_constraint.letter] !== _constraint.value,
         _constraints,
       );

let findWordWithConstraints = (dict: WordDict.t, len: int, _constraints: list(letterConstraint), reject: list(string)): option(WordDict.entry) => {
  let candidates = WordDict.get(dict, len);
  switch (candidates |> List.find((entry: WordDict.entry) => fitsConstraints(_constraints, reject, entry.word))) {
  | match => Some(match)
  | exception Not_found => None
  };
};

let calcConstraints = (wordShape: partialWord, fittedWords: list(completedWord)): list(letterConstraint) => {
  fittedWords |> List.fold_left((constraintList, fittedWord): list(letterConstraint) => {
    switch (fittedWord) {
    | f when f.dir === wordShape.dir => constraintList
    | f when fittedWord.crossIndex < wordShape.start => constraintList
    | f when fittedWord.crossIndex >= wordShape.start + wordShape.length => constraintList
    | f when wordShape.crossIndex - fittedWord.start < 0 => constraintList
    | f when wordShape.crossIndex - fittedWord.start >= String.length(fittedWord.value) => constraintList
    | f => [{ letter: f.crossIndex - wordShape.start, value: String.get(fittedWord.value, wordShape.crossIndex - fittedWord.start) }, ...constraintList]
    }
  }, []);
};

/* function fitWords(wordShape, dict) {
  const partialSolutions = [{ fittedWords: [], reject: [], timesFitted: 0 }];
  let currentWordInd = 0;
  let latestSolution = partialSolutions[currentWordInd];

  while (currentWordInd < wordShape.length) {
    console.log(`Fit ${currentWordInd} words so far.`);
    const thisWordShape = wordShape[currentWordInd];
    const { fittedWords, reject, timesFitted } = latestSolution;
    const constraints = calcConstraints(thisWordShape, fittedWords);
    const nextWord = findWordWithConstraints(
      dict,
      thisWordShape.length,
      constraints,
      reject
    );
    if (nextWord && timesFitted < MAX_TIMES_FITTED) {
      latestSolution.timesFitted += 1;
      latestSolution = {
        fittedWords: latestSolution.fittedWords.concat(
          Object.assign({}, thisWordShape, { value: nextWord })
        ),
        reject: [],
        timesFitted: 0
      };
      partialSolutions.push(latestSolution);
      currentWordInd += 1;
    } else {
      if (timesFitted >= MAX_TIMES_FITTED)
        console.log(`Rejecting due to failure after ${timesFitted} failures.`);
      const rejectWord = fittedWords.slice(-1)[0];
      if (!rejectWord) {
        throw new Error('No solution can be found');
      }
      partialSolutions.pop();
      currentWordInd -= 1;
      latestSolution = partialSolutions[currentWordInd];
      latestSolution.reject.push(rejectWord.value);
    }
  }

  return latestSolution.fittedWords;
} */

type readerInput =
  | Filename(string)
  | FilenameList(list(string));

let rec findStringMatches = (~find: Str.regexp, ~input: string, ~matches: list(int)=[], ~start: int=0, ()): list(int) => {
  switch (Str.search_forward(find, input, start)) {
  | n => findStringMatches(~find=find, ~input=input, ~matches=[n, ...matches], ~start=(n + 1), ())
  | exception Not_found => matches
  };
};

let parseLine = (line: string): WordDict.entry => {
  let breaks = findStringMatches(~find=Str.regexp("_"), ~input=line, ());
  let length = String.length(line);
  {
    word: line,
    breaks: breaks,
    length: length
  };
};

let buildDict = (wordList: list(WordDict.entry)): WordDict.t => {
  let wordDict = ref(WordDict.init());
  wordList |> List.iter((entry) => {
    wordDict := WordDict.add(wordDict^, entry);
  });
  wordDict^;
};

let rec readWordList = (filename: readerInput): list(WordDict.entry) => {
  switch(filename) {
  | FilenameList(filenameList) => {
    filenameList |> List.fold_left((dataSoFar, filename) => List.append(readWordList(Filename(filename)), dataSoFar), []);
  }
  | Filename(filename) => {
    let chan = open_in(filename);
    let wordList = ref([]);
    let break = ref(false);

    while (! break^) {
      switch(input_line(chan)) {
      | line => wordList := [parseLine(line), ...wordList^]
      | exception End_of_file => break := true
      };
    };
    wordList^;
  }
  }
};
