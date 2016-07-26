*GT-InspectorExtensions-Core
gtInspectorItemsIn: composite
	<gtInspectorPresentationOrder: 0>
	composite fastList
		title: 'Items';
		display: [ items copy ];
		format: [ :each | each gtDisplayString ]