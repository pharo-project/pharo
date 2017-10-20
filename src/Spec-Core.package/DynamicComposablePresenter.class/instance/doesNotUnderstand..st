error handling
doesNotUnderstand: aMessage
	"To make sure that we can use accessors to access the dynamically managed subcomponents."
	
	^ widgets 
			at: aMessage selector 
			ifAbsent: [ super doesNotUnderstand: aMessage ]