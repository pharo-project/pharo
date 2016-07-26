testing
testNonExistingClass
	| newClass |
	
	newClass:= RGTraitDefinition named:  #TConnection.
	self assert: (newClass isTrait).
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