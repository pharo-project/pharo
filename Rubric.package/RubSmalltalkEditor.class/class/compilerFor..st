accessing
compilerFor: aClass
	^ (aClass respondsTo: #compiler) 
		ifTrue: [aClass compiler]
		ifFalse: [ aClass class evaluatorClass new ]