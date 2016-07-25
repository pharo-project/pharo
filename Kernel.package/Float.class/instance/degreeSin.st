mathematical functions
degreeSin
	"Take care of exceptional values"
	
	self isFinite ifTrue: [^super degreeSin].
	^self degreesToRadians sin