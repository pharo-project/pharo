inspecting
gtInspectorProcessorsIn: composite
	<gtInspectorPresentationOrder: 40>
	composite list
		title: 'Processors';
		display: [ self processors ];
		format: #gtDisplayString;
		when: [ self spotter notNil ]