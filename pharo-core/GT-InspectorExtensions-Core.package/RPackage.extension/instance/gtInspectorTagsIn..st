*GT-InspectorExtensions-Core
gtInspectorTagsIn: composite
	<gtInspectorPresentationOrder: 40>
	composite list
		title: 'Tags';
		display: #classTags;
		format: #gtDisplayString;
		when: [ self classTags notEmpty ]