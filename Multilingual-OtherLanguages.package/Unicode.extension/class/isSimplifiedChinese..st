*Multilingual-OtherLanguages
isSimplifiedChinese: code

	^ code > 255 and: [(GB2312 charFromUnicode: code) notNil]


