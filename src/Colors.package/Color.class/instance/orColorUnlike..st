transformations
orColorUnlike: theOther
	"If this color is a lot like theOther, then return its complement, otherwide, return self"

	^ (self diff: theOther) < 0.3
		ifTrue: [ theOther negated ]
		ifFalse: [ self ]