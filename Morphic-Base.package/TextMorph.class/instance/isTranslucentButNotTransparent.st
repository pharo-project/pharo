accessing
isTranslucentButNotTransparent
	"Overridden from BorderedMorph to test backgroundColor instead of (text) color."

	backgroundColor ifNil: [ ^ true ].
	(backgroundColor isColor and: [ backgroundColor isTranslucentButNotTransparent ])
		ifTrue: [ ^ true ].
	(borderColor isColor and: [ borderColor isTranslucentButNotTransparent ])
		ifTrue: [ ^ true ].
	^ false