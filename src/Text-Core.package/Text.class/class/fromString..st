instance creation
fromString: aString 
  "Answer an instance of me whose characters are those of the argument, aString."

  ^ self
    string: aString
    attributes:{ "No default attributes" }
