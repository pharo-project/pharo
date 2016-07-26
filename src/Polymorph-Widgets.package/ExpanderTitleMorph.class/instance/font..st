accessing
font: aFont
	"Set the label font"

	((self labelMorph isKindOf: StringMorph) or: [self labelMorph isTextMorph])
		ifTrue: [self labelMorph font: aFont]