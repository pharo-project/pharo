converting
withNegativeSign
	"Same as super, but handle the subtle case of Float negativeZero"
	
	self = 0.0 ifTrue: [^self class negativeZero].  
	^super withNegativeSign