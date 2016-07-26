filein/out
installNewFontAtIndex: newIndex fromOld: oldIndex

	
	self allInstances do: [:set | | newArray fontArray |
		fontArray := set fontArray.
		newIndex + 1 > fontArray size ifTrue: [
			newArray := Array new: newIndex + 1.
			newArray replaceFrom: 1 to: fontArray size with: fontArray startingAt: 1.
			newArray at: newIndex + 1 put: (fontArray at: oldIndex + 1).
			set initializeWithFontArray: newArray.
		] ifFalse: [
			fontArray at: newIndex + 1 put: (fontArray at: oldIndex + 1).
		].
	].

"
StrikeFontSet installNewFontAtIndex: UnicodeSimplifiedChinese leadingChar fromOld: UnicodeJapanese leadingChar
StrikeFontSet installNewFontAtIndex: UnicodeKorean leadingChar fromOld: UnicodeJapanese leadingChar
"
