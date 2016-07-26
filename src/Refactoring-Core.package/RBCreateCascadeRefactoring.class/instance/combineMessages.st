transforming
combineMessages
	"This combines the messages and adds the assignements of the last statement to the cascade. This is not necessary if there is a return, because the refactoring engine automatically compensates for that."

	| expression |
	transformedNode := RBCascadeNode messages: (statementNodes
		collect: [ :each | each copy ]).
	expression := statementNodes last parent.
	[ expression isAssignment ] whileTrue: [
		transformedNode := RBAssignmentNode
			variable: expression variable
			value: transformedNode.
		expression := expression parent ]