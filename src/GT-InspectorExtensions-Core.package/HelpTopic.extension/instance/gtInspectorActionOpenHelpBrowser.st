*GT-InspectorExtensions-Core
gtInspectorActionOpenHelpBrowser
	<gtInspectorAction>
	^ GLMGenericAction new
		action: [ HelpBrowser openOn: self ];
		icon: (Smalltalk ui icons iconNamed: #bookIcon);
		title: 'Browse'