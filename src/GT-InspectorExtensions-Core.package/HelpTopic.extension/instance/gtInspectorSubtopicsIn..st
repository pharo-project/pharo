*GT-InspectorExtensions-Core
gtInspectorSubtopicsIn: composite
	<gtInspectorPresentationOrder: 50>

	composite tree
		shouldValidate: false;
		title: 'Subtopics';
		display: [ self subtopics ];
		children: [ :each | each subtopics ];
		when: [ self hasSubtopics ];
		format: #title