testing
testExplicitRequirementTakesPrecedenceOverTraitImplementation
	"If i create an explicit requirement method on a trait, the method has to be explicit requirement "
	self t12 compile: 'm ^11'.
	self t11 compile: 'm ^self explicitRequirement'.
	
	self should: [ self c11 new m ] raise: Error.
	
	"If i create an explicit requirement method on a class, and the superclass already have the method (non explicit requirement), the class should raise an exception"
	self c10 compile: 'm ^111'.
	self c11 compile: 'm ^self explicitRequirement'.
	self should: [ self c11 new m ] raise: Error.
	
	