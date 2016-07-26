transforming
modifyImplementorParseTree: parseTree in: aClass 
	| node assignment |
	node := (parseTree arguments at: parameterIndex) copy.
	parseTree body addTemporaryNamed: node name.
	assignment := RBAssignmentNode variable: node copy value: expressions first.
	parseTree body addNodeFirst: assignment.
	super modifyImplementorParseTree: parseTree in: aClass