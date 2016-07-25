generic parent api
realParent
	"Retrieves the Class/Trait/.. object in the System corresponding to the class of the this element."
	
	^self parent notNil
		ifTrue: [ self parent realClass ]
		ifFalse: [ self rootEnvironment classNamed: self parentName ]