filein/out
removeFontsForEncoding: leadingChar encodingName: encodingSymbol

	| insts |
	leadingChar = 0 ifTrue: [^ self error: 'you cannot delete the intrinsic fonts'].
	insts := self allInstances.
	insts do: [:inst | | index fonts newFonts |
		fonts := inst fontArray.
		fonts size >= (leadingChar + 1) ifTrue: [
			leadingChar + 1 = fonts size ifTrue: [
				newFonts := fonts copyFrom: 1 to: fonts size - 1.
				index := newFonts indexOf: nil.
				index > 0 ifTrue: [newFonts := newFonts copyFrom: 1 to: index - 1].
				inst initializeWithFontArray: newFonts.
			] ifFalse: [
				fonts at: leadingChar + 1 put: nil.
			].
		].
	].

	TextSharedInformation removeKey: encodingSymbol asSymbol ifAbsent: [].
