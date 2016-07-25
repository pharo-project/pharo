accessing
inverseTransformation
	"Return the inverse transformation of the receiver"
	^MorphicTransform
		offset: (self transform: 0@0) - (self transform: offset)
		angle: angle negated
		scale: scale reciprocal