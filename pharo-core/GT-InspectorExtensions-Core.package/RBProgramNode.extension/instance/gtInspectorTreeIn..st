*GT-InspectorExtensions-Core
gtInspectorTreeIn: composite
	<gtInspectorPresentationOrder: 30> 
	^ (GTSimpleRBTreeBrowser new treeIn: composite)
		title: 'Tree'