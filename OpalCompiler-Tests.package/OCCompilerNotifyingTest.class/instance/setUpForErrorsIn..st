initialize-release
setUpForErrorsIn: aTextWithErrorsEnclosedInBackQuote
  "Extract the expectedErrors, the expectedErrorPositions and set up a TextMorph containing the text without errors.
  each section separated by % in aTextWithErrorsEnclosedInBackQuote will be evaluated separately.
  The expected error message should lie in aTextWithErrorsEnclosedInBackQuote at the expected position, and enclosed in back quotes."
  text := aTextWithErrorsEnclosedInBackQuote.
  morph := MockSourceEditor new contents: self initializeTextWithoutError asString.