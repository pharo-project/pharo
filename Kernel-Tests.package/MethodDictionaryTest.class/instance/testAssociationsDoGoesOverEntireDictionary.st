tests - others
testAssociationsDoGoesOverEntireDictionary
	| associations |
	associations := MethodDictionary new.
	
	self class methodDict associationsDo:[ :association | associations add: association ].
	
	self assertPreservesElements: self class methodDict comparedTo: associations.