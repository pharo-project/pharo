*GT-InspectorExtensions-Core
gtInspectorMappingsIn: composite
	<gtInspectorPresentationOrder: 40>
	composite list
		title: 'Keys';
		display: [ self allEntries keymaps ];
		filterOn: [ :text :each | each gtDisplayString includesSubstring: text caseSensitive: false ];
		format: #gtDisplayString;
		when: [ self allEntries keymaps notEmpty ]