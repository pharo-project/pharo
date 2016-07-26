accessing
font
	"Answer the label font"

	^((self labelMorph isKindOf: StringMorph) or: [self labelMorph isTextMorph])
		ifTrue: [self labelMorph font]