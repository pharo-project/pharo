testing
testExplicitRequirementWithSuperclassImplementationAndAnotherTrait
	"Make sure that an explicitRequirement method from a trait does not take precedence over a superclass implementation."

	"The previous should still be true even in the presence of a superclass importing another explicitRequirement method."

	"C9 inherits from Object. C10 inherits from C9. C11 inherits from C10. Each Ci uses the trait Ti."

	self t10 compile: 'm ^self explicitRequirement'.
	self t11 compile: 'm ^self explicitRequirement'.
	self should: [ self c10 new m ] raise: Error.
	self should: [ self c11 new m ] raise: Error.
	self t9 compile: 'm ^2'.
	self assert: self c9 new m equals: 2.
	self assert: self c10 new m equals: 2.
	self assert: self c11 new m equals: 2