converting
asIntegerSegment
	"Convert the receiver into integer representation"
	^self species 
			from: start asIntegerPoint 
			to: end asIntegerPoint 
			via: via asIntegerPoint