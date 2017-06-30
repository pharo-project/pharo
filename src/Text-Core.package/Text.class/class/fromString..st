instance creation
fromString: aString 
  "Answer an instance of me whose characters are those of the argument, aString."

  | defaultFontStyle fontNumber |

  defaultFontStyle := TextStyle default.
  fontNumber := defaultFontStyle ifNil: [1] ifNotNil: [defaultFontStyle defaultFontIndex].

  ^ self
    string: aString
    attributes:{ "No default attributes" }
