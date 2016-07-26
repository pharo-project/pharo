*GT-InspectorExtensions-Core
gtInspectorTreeIn: composite
	<gtInspectorPresentationOrder: 0>
	composite tree
		title: 'Tree';
		display: [ { self } ]	;
		children: [ :each | each gtInspectorChildren ]