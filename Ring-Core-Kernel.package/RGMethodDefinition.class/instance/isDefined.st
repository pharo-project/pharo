testing
isDefined
	"Answer whether the receiver exists in the environment"
	
	| rClass |
	self selector isDoIt ifTrue:[ ^false ].
	rClass := self realClass.
	^rClass notNil and: [ rClass includesSelector: self selector ]