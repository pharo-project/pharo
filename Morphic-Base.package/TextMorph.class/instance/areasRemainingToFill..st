drawing
areasRemainingToFill: aRectangle
	"Overridden from BorderedMorph to test backgroundColor instead of (text) color."
	(backgroundColor isNil or: [backgroundColor isTranslucent])
		ifTrue: [^ Array with: aRectangle].
	self wantsRoundedCorners
	ifTrue: [(borderWidth > 0 and: [borderColor isColor and: [borderColor isTranslucent]])
				ifTrue: [^ aRectangle areasOutside: (self innerBounds intersect: self boundsWithinCorners ifNone: [self error: 'cannot happen' ])]
				ifFalse: [^ aRectangle areasOutside: self boundsWithinCorners]]
	ifFalse: [(borderWidth > 0 and: [borderColor isColor and: [borderColor isTranslucent]])
				ifTrue: [^ aRectangle areasOutside: self innerBounds]
				ifFalse: [^ aRectangle areasOutside: self bounds]]