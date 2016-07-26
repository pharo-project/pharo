*GT-InspectorExtensions-Core
gtInspectorActionLoad
	<gtInspectorAction>
	^ GLMGenericAction new
		action: [ self load ];
		icon: (Smalltalk ui icons iconNamed: #smallLoadProject);
		title: 'Load'