accessing
value: anObject 
	"Store the argument, anObject, as the value of the receiver."

	anObject == self ifTrue: [self error: 'Using the WeakAssociation as its own value'].
	value := anObject