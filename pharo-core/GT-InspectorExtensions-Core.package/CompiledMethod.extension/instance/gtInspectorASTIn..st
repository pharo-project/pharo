*GT-InspectorExtensions-Core
gtInspectorASTIn: composite
	<gtInspectorPresentationOrder: 35> 
	(GTSimpleRBTreeBrowser new treeIn: composite)
		title: 'AST';
		display: [ :anObject | {anObject ast} ]
		