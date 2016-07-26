*GT-InspectorExtensions-Core
gtInspectorContentsIn: composite
	<gtInspectorPresentationOrder: 40>
	
	composite text
		title: 'Contents';
		display: [ self contents ];
		when: [ self contents notEmpty  ] 