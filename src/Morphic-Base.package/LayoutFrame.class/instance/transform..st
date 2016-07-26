transforming
transform: outerReferenceRectangle
	"Answer a new rectangle which transformed by receiver relative to outerReferenceRectangle.
	If it is not possible to place a outerReferenceRectangle *inside*  this reference rectangle, then result will be zero-width (or zero-height) rectangle. Basically you transform to obtain a subrectangle area inside the reference rectangle"
	
	|  left right top bottom |
	left := (outerReferenceRectangle left + (outerReferenceRectangle width * leftFraction) + leftOffset) rounded asInteger.
	right := (outerReferenceRectangle right - (outerReferenceRectangle width * (1 - rightFraction)) + rightOffset) rounded asInteger.
	top:= (outerReferenceRectangle top + (outerReferenceRectangle height * topFraction) + topOffset ) rounded asInteger.
	bottom:= (outerReferenceRectangle bottom - (outerReferenceRectangle height * (1 - bottomFraction)) + bottomOffset) rounded asInteger. 
	
	right < left ifTrue: [ right := left ].
	bottom < top ifTrue: [ bottom := top ].
	
	^ Rectangle left: left right: right top: top bottom: bottom