visiting
visitAssignmentNode: anAssignmentNode
	"search registered argument variable with the same name and scope"

	| varName |
	super visitAssignmentNode: anAssignmentNode.
	varName := anAssignmentNode variable name.
	arguments
		detect: [ :elem | elem name = varName and: [ self is: anAssignmentNode variable inScopeOf: elem ] ]
		ifFound: [ argumentAssignments add: varName ]