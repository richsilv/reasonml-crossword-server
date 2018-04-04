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
  breaks: list(int)
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

  (words^, grid^);
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

type partialSolution = {
  fittedWords: list(completedWord),
  reject: list(string),
  timesFitted: int
};

let maxTimesFitted = 10;

let fitWords = (wordShapes: list(partialWord), dict: WordDict.t): list(completedWord) => {
  let currentWordInd = ref(0);
  let latestSolution = ref({ fittedWords: [], reject: [], timesFitted: 0 });
  let partialSolutions = ref([latestSolution^]);
  let targetLength = List.length(wordShapes);

  while (currentWordInd^ < targetLength) {
    print_int(currentWordInd^);
    let thisWordShape = List.nth(wordShapes, currentWordInd^);
    let { fittedWords, reject, timesFitted } = latestSolution^;
    let theseConstraints = calcConstraints(thisWordShape, fittedWords);
    let nextWord = findWordWithConstraints(dict, thisWordShape.length, theseConstraints, reject);
    switch (nextWord) {
    | Some(word) when timesFitted < maxTimesFitted => {
      let newWord = {
        value: word.word,
        breaks: word.breaks,
        dir: thisWordShape.dir,
        col: thisWordShape.col,
        row: thisWordShape.row,
        start: thisWordShape.start,
        crossIndex: thisWordShape.crossIndex,
        length: thisWordShape.length
      };
      latestSolution := {
        fittedWords: [newWord, ...fittedWords],
        reject: [],
        timesFitted: 0
      };
      partialSolutions := [latestSolution^, { fittedWords, reject, timesFitted: timesFitted + 1 }, ...List.tl(partialSolutions^)];
      currentWordInd := currentWordInd^ + 1;
    }
    | _ => {
      if (timesFitted >= maxTimesFitted) {
        print_string("Rejecting due to failure");
      };
      let rejectWord = List.hd(fittedWords);
      currentWordInd := currentWordInd^ - 1;
      switch (partialSolutions^) {
      | [_, tempSolution, ...tailSolutions] => {
        latestSolution := { ...tempSolution, reject: [rejectWord.value, ...tempSolution.reject] };
        partialSolutions := [latestSolution^, ...tailSolutions];
      }
      | _ => failwith("Cannot fit words.");
      }
    }
    };
  };
  latestSolution^.fittedWords;
};

type readerInput =
  | Filename(string)
  | FilenameList(list(string));

let rec findStringMatches = (~find: Str.regexp, ~input: string, ~matches: list(int)=[], ~start: int=0, ()): list(int) => {
  switch (Str.search_forward(find, input, start)) {
  | n => findStringMatches(~find=find, ~input=input, ~matches=[n, ...matches], ~start=(n + 1), ())
  | exception Not_found => matches
  };
};

let parseWord = (word: string): WordDict.entry => {
  let breaks = findStringMatches(~find=Str.regexp("_"), ~input=word, ());
  let length = String.length(word);
  {
    word: word,
    breaks: breaks,
    length: length
  };
};

let parseLine = (line: string): option(WordDict.entry) => {
  switch(Str.split(Str.regexp("[\t]+"), line)) {
  | [word] => Some(parseWord(word))
  | [word, count] => Some(parseWord(word))
  | _ => None
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
      | line => {
        switch (parseLine(line)) {
        | Some(newWord) => wordList := [newWord, ...wordList^]
        | None => ()
        };
      }
      | exception End_of_file => break := true
      };
    };
    wordList^;
  }
  }
};
