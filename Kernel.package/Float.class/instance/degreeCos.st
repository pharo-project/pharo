mathematical functions
degreeCos
	"Take care of exceptional values"
	
	self isFinite ifTrue: [^super degreeCos].
	^self degreesToRadians cos