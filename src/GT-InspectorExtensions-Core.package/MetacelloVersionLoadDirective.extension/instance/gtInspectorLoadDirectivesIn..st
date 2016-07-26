*GT-InspectorExtensions-Core
gtInspectorLoadDirectivesIn: composite
	<gtInspectorPresentationOrder: 1>
	composite tree
		title: 'Load directives';
		display: [ { self } ];
		format: [:each | each label ifEmpty: [each title]];
		rootsExpanded;
		children: #gtInspectorChildren;
		filterOn: [:text :each | '*', text, '*' match: each label ]