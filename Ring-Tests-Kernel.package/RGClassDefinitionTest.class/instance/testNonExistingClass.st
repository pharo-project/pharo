testing
testNonExistingClass
	| newClass |
	
	newClass:= RGClassDefinition named:  #Connection.
	self assert: (newClass isClass).
	self assert: (newClass instanceVariables isEmpty).
	self assert: (newClass classVariables isEmpty).
	self assert: (newClass sharedPools isEmpty).
	self assert: (newClass hasMetaclass not).
	self assert: (newClass hasComment not).
	self assert: (newClass hasStamp not).
	self assert: (newClass parent = Smalltalk globals).
	self assert: (newClass package isNil).
	self assert: (newClass category isNil).
	self assert: (newClass hasMethods not).
	self assert: (newClass hasSuperclass not).
	self assert: (newClass hasTraitComposition not).
	self assert: (newClass isDefined not).
	self assert: (newClass hasProtocols not).
	
	