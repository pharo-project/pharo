*Multilingual-OtherLanguages
isKorean: code

	^ code > 255 and: [(KSX1001 charFromUnicode: code) notNil]

