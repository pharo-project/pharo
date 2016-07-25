testing
testExplicitRequirementDoesNotTakePrecedenceInDeepHierarchy
	"Make sure that an explicitRequirement method from a trait does not take precedence over a superclass implementation, even if the superclass implementation is not just in the direct superclass."

	"C9 inherits from Object. C10 inherits from C9. C11 inherits from C10. Each Ci uses the trait Ti."

	self t11 compile: 'm ^self explicitRequirement'.
	self should: [ self c11 new m ] raise: Error.
	self t9 compile: 'm ^2'.
	self c9 new m.
	self c10 new m.
	self c11 new m.
	self assert: self c9 new m equals: 2.
	self assert: self c10 new m equals: 2.
	self assert: self c11 new m equals: 2