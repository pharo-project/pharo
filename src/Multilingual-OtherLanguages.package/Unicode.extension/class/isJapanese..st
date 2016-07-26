*Multilingual-OtherLanguages
isJapanese: code

	^ code > 255 and: [(JISX0208 charFromUnicode: code) notNil].
