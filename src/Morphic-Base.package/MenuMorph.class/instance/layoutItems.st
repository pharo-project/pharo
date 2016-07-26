control
layoutItems
	"decorate aMenu with icons"

	| maxIconWidth |
	maxIconWidth := 0.

	self items do: [:item | 
		item icon ifNotNil: [maxIconWidth := maxIconWidth max: item icon width].
		item hasSubMenu ifTrue: [item subMenu layoutItems]].
	maxIconWidth isZero 
		ifFalse: [self addBlankIconsIfNecessary: (Smalltalk ui icons blankIconOfWidth: maxIconWidth)].
