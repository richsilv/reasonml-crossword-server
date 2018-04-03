type entry = {
  word: string,
  breaks: list(int),
  length: int
};

type entryList = list(entry);

module IntDict =
Map.Make(
    {
      type t = int;
      let compare = compare;
    },
    );

type t = IntDict.t(entryList);

let init = () => IntDict.empty;

let get = (wordDict: IntDict.t(entryList), len: int): entryList =>
  switch (IntDict.find(len, wordDict)) {
  | entryList => entryList
  | exception Not_found => []
  };

let add = (wordDict: IntDict.t(entryList), word: entry): t => {
  let entryList = get(wordDict, word.length);
  IntDict.add(word.length, [word, ...entryList], wordDict);
};

