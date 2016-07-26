rounding
roundedCornersString
	"Answer the string to put in a menu that will invite the user to 
	switch to the opposite corner-rounding mode"
	^ (self wantsRoundedCorners
		ifTrue: ['<yes>' translated]
		ifFalse: ['<no>' translated])
		, ('round corners' translated)