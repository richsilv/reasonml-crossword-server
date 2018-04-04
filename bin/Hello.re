print_string("foobar");

let dict = Crossword.CrosswordMaker.buildDict(Crossword.CrosswordMaker.readWordList(Crossword.CrosswordMaker.FilenameList([
  "/home/richard/Personal/crossword/wordData/adjectives.txt",
  "/home/richard/Personal/crossword/wordData/adverbs.txt",
  "/home/richard/Personal/crossword/wordData/countries.txt",
  "/home/richard/Personal/crossword/wordData/englishCounties.txt",
  "/home/richard/Personal/crossword/wordData/nationalCapitals.txt",
  "/home/richard/Personal/crossword/wordData/nouns.txt",
  "/home/richard/Personal/crossword/wordData/usStateCapitals.txt",
  "/home/richard/Personal/crossword/wordData/usStates.txt",
  "/home/richard/Personal/crossword/wordData/verbs.txt"
  ])));

let (wordShapes, grid) = Crossword.CrosswordMaker.makeGrid(12);

let crossword = Crossword.CrosswordMaker.fitWords(wordShapes, dict);

print_string("cheeseman");
