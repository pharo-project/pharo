accessing methods
compiledMethodNamed: selector
	"Retrieves the compiled method from aRGMethodDefinition"
	| method |

	^(method:= self methodNamed: selector) notNil
	ifTrue: [ method compiledMethod ]
	ifFalse:[ nil ]