inspecting
gtInspectorStepsIn: composite
	<gtInspectorPresentationOrder: 40>
	composite list
		title: 'Steps';
		display: [ self steps ];
		when: [ self steps notEmpty ]