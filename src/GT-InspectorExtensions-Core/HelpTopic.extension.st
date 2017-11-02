Extension { #name : #HelpTopic }

{ #category : #'*GT-InspectorExtensions-Core' }
HelpTopic >> gtInspectorActionOpenHelpBrowser [
	<gtInspectorAction>
	^ GLMGenericAction new
		action: [ HelpBrowser openOn: self ];
		icon: (Smalltalk ui icons iconNamed: #bookIcon);
		title: 'Browse'
]

{ #category : #'*GT-InspectorExtensions-Core' }
HelpTopic >> gtInspectorContentsIn: composite [
	<gtInspectorPresentationOrder: 40>
	
	composite text
		title: 'Contents';
		display: [ self contents ];
		when: [ self contents notEmpty  ] 
]

{ #category : #'*GT-InspectorExtensions-Core' }
HelpTopic >> gtInspectorSubtopicsIn: composite [
	<gtInspectorPresentationOrder: 50>

	composite tree
		shouldValidate: false;
		title: 'Subtopics';
		display: [ self subtopics ];
		children: [ :each | each subtopics ];
		when: [ self hasSubtopics ];
		format: #title
]
