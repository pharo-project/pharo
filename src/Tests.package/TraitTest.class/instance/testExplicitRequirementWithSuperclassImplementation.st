testing
testExplicitRequirementWithSuperclassImplementation
	"Make sure that an explicitRequirement method from a trait does not take precedence over a superclass implementation."

	self t10 compile: 'm ^self explicitRequirement'.
	self should: [ self c10 new m ] raise: Error.
	self c9 compile: 'm ^123'.
	self assert: self c10 new m equals: 123.
	self c9 removeSelector: #m.
	self t10 removeSelector: #m.	"The previous is still true even if we compile the superclass method before the trait method."
	self c9 compile: 'm ^123'.
	self t10 compile: 'm ^self explicitRequirement'.
	self c10 new m.
	self c9 removeSelector: #m.
	self should: [ self c10 new m ] raise: Error.
	self t10 removeSelector: #m