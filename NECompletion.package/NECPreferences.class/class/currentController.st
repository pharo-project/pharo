private
currentController 
	^ (Smalltalk tools hasToolNamed: #codeCompletion)
		ifTrue: [ Smalltalk tools codeCompletion ]
		ifFalse: [ nil ]