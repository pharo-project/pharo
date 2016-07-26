*OpalCompiler-Core
findGlobalVariable: lookupBlock ifNone: notFoundBlock 
	| var |
	var := OCLiteralVariable new.
	var scope: self.
			
	self classPool associationsDo: [ :assoc |
		var assoc: assoc.
		(lookupBlock value: var) ifTrue: [ ^ var ]].
	
	self sharedPools do: [ :pool |
		pool classPool associationsDo: [ :assoc | 
			var assoc: assoc.
			(lookupBlock value: var) ifTrue: [ ^ var ]]].

	self environment associationsDo: [ :assoc |
		var assoc: assoc.
		(lookupBlock value: var) ifTrue: [ ^ var ] ].
	
	^ notFoundBlock value