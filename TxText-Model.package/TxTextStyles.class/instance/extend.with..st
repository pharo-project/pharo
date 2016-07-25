operations
extend: style with: aNewAttribute
	"Answer a style, which is the same as the first argument, but with a new/different attribute value."
	| newStyle |
	(style includes: aNewAttribute) ifTrue: [ ^ style ].
	newStyle := style copy add: aNewAttribute.
	^ self addStyle: newStyle