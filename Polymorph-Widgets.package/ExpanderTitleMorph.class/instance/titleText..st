operations
titleText: aStringOrText
	"Set the text if the title morph is capable."

	((self labelMorph isKindOf: StringMorph) or: [self labelMorph isTextMorph])
		ifTrue: [self labelMorph contents: aStringOrText]