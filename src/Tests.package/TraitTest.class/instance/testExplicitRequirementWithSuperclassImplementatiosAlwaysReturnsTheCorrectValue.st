testing
testExplicitRequirementWithSuperclassImplementatiosAlwaysReturnsTheCorrectValue
	"Make sure that an explicitRequirement method from a trait that is overrided by a method on a superclass always returns the value from the class defined method, even when an explict return from the trait defined method is missing."

	"C9 inherits from Object. C10 inherits from C9.  Each Ci uses the trait Ti."

	self t10 compile: 'm1 ^self explicitRequirement'.
	self t10 compile: 'm2 self explicitRequirement'.
	self t9 compile: 'm1 ^2'.
	self t9 compile: 'm2 ^2'.
	
	self assert: self c10 new m1 equals: 2.
	self assert: self c10 new m2 equals: 2.