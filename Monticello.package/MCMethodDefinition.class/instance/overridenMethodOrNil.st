installing
overridenMethodOrNil
	"Answer the method that I've overriden on load. 

	Preconditions: 
	  - self actualClass is installed in the System Dictionary.
	  - self isOverrideMethod is true."
	
	| realMethod |
	realMethod := self actualClass
		compiledMethodAt: self selector
		ifAbsent: [ ^ nil ].
		
	^ SourceFiles
		changeRecordsFor: realMethod asRingDefinition
		detect: [ :protocol | protocol ~= category ]