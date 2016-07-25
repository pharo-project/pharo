protocol
titleText
	"Answer the text if the title morph is capable."

	^((self labelMorph isKindOf: StringMorph) or: [self labelMorph isTextMorph])
		ifTrue: [self labelMorph contents]
		ifFalse: ['']