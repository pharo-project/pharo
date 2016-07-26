*GT-InspectorExtensions-Core
gtInspectorPreviewIn: composite
	composite list 
		title: 'Changes';
		display: [ self changes ];
		when: [ self changes notEmpty ]