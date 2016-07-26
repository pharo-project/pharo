system primitives
endsWithAColon 
	"Answer whether the final character of the receiver is a colon
	
	#fred: endsWithAColon
	'fred' endsWithAColon
	"

	^ self size > 0 and: [self last == $:]
