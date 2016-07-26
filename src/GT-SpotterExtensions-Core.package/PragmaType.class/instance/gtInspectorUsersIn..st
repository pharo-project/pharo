ui
gtInspectorUsersIn: composite
	<gtInspectorPresentationOrder: 1>
	composite list
		title: 'Users';
		display: [ self users sorted: [ :a :b | a selector < b selector ] ];
		format: #gtDisplayString