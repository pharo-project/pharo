*Refactoring-Critics
testExplicitRequirementMethodsShouldBeImplementedInTheirUsers
	"If a class is using a trait with an explicit requirement method, this class should implement the method"
	| result |
	result := RBExplicitRequirementMethodsRule new run.
	self assert: result isEmpty.