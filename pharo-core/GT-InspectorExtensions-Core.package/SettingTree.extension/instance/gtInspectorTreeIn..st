*GT-InspectorExtensions-Core
gtInspectorTreeIn: composite
	<gtInspectorPresentationOrder: 0>
	composite tree 
		title: 'Tree';
		display: [ self settingTreeRoots ];
		children: [ :each | self childrenOf: each ];
		format: [ :each | each settingDeclaration label ] 