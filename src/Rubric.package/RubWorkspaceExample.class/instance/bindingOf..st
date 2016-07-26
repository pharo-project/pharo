shout
bindingOf: aString 
	
	mustDeclareVariables ifTrue: [^ nil].
	"I want to have workspace that force the user to declare  
	variables. Still subclasses may want to do something else"
	bindings isNil
		ifTrue: [self initializeBindings].
	(bindings includesKey: aString)
		ifFalse: [bindings at: aString put: nil].
	^ bindings associationAt: aString