*GT-InspectorExtensions-Core
gtInspectorCodeIn: composite
	<gtInspectorPresentationOrder: 40>
	composite pharoScript 
		title: 'Code';
		display: [ 
			String streamContents: [ :s | self configMethodOn: s indent: 0] ]
