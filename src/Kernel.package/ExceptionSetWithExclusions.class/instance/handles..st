exceptionselector
handles: exception
	"Return true when I will handled exception.
	I extend my superclass behavior by explicitely 
	not handling a number of exclusion exceptions."

	^ (super handles: exception) 
			and: [ exclusions noneSatisfy: [ :each | each handles: exception ] ]