Extension { #name : #MetacelloVersionLoadDirective }

{ #category : #'*GT-InspectorExtensions-Core' }
MetacelloVersionLoadDirective >> gtInspectorChildren [
	^ self loadDirectives
]

{ #category : #'*GT-InspectorExtensions-Core' }
MetacelloVersionLoadDirective >> gtInspectorLoadDirectivesIn: composite [
	<gtInspectorPresentationOrder: 1>
	composite tree
		title: 'Load directives';
		display: [ { self } ];
		format: [:each | each label ifEmpty: [each title]];
		rootsExpanded;
		children: #gtInspectorChildren;
		filterOn: [:text :each | '*', text, '*' match: each label ]
]
