testing
isLocalSelector
	"Answer whether the receiver exists in the environment as a local implementation"
	
	| rClass |
	rClass:= self realClass.
	^rClass notNil and: [ rClass includesLocalSelector: self selector ]