inspecting
gtInspectorExtensionsIn: composite
	<gtInspectorPresentationOrder: 40>
	composite list
		title: 'Extensions';
		display: [ self inspectorExtendingMethods ];
		showOnly: 50;
		format: [ :each | each gtDisplayString ];
		filterOn: [ :text :each | each gtDisplayString includesSubstring: text caseSensitive: false ]