private
defaultFontChange
  "Answer a TextFontChange that represents the default font"

  | defaultFontStyle fontIndex |

  defaultFontStyle := TextStyle default.
  fontIndex := defaultFontStyle ifNil: [1] ifNotNil: [defaultFontStyle defaultFontIndex].

  ^ self new fontNumber: fontIndex.
