testing
testExplicitRequirementDoesNotTakePrecedenceEvenWhenAddingTraits
	"Make sure that an explicitRequirement method from a trait does not take precedence over a superclass implementation, even if he trait is created later."

	| tempClass tempTrait |
	self c9 compile: 'm ^100'.
	tempTrait := self createTraitNamed: #TTempTrait uses: {}.
	tempTrait compile: 'm ^self explicitRequirement.'.
	tempClass := self createClassNamed: #TempClass superclass: self c9 uses: {tempTrait}.
	self assert: tempClass new m equals: 100