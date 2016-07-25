accessing
compiledMethod
	"Retrieves the compiled method of this definition if exists"
	
	| rClass |
	(rClass := self realClass) notNil
		ifTrue: [ (rClass includesSelector: self selector)
			ifTrue: [ ^rClass >> self selector ] ].
			
	^nil