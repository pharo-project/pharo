drawing
areasRemainingToFill: aRectangle 
	"Pushed up from BorderedMorph, all cases tested for there are
	supported by basic Morph."
	"Morphs which achieve translucency by other means than fillStyle will have
	to reimplement this"
	"Fixed here to test the fillStyle rather than color for translucency.
	Since can have a translucent fillStyle while the (calculated) color is not."
	self fillStyle isTranslucent
		ifTrue: [^ Array with: aRectangle].
	self wantsRoundedCorners
		ifTrue: [(self borderWidth > 0
					and: [self borderColor isColor
							and: [self borderColor isTranslucent]])
				ifTrue: [^ aRectangle
						areasOutside: (self innerBounds intersect: self boundsWithinCorners)]
				ifFalse: [^ aRectangle areasOutside: self boundsWithinCorners]]
		ifFalse: [(self borderWidth > 0
					and: [self borderColor isColor
							and: [self borderColor isTranslucent]])
				ifTrue: [^ aRectangle areasOutside: self innerBounds]
				ifFalse: [^ aRectangle areasOutside: self bounds]]