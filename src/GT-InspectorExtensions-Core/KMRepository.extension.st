Extension { #name : #KMRepository }

{ #category : #'*GT-InspectorExtensions-Core' }
KMRepository >> gtInspectorGlobalsIn: composite [
	<gtInspectorPresentationOrder: 40>
	composite table 
		title: 'Global keys';
		display: [ 
			self globalCategories
				flatCollect: [ :category | 
					category allEntries keymaps collect: [ :key | key -> category ] ]
				 ];
		filterOn: [ :text :each | each gtDisplayString includesSubstring: text caseSensitive: false ];
		column: 'Shortcut' evaluated: [ :assoc | assoc key shortcut gtDisplayString ] width: 120;
		column: 'Description' evaluated: [ :assoc | assoc key description ];
		column: 'Action' evaluated: [ :assoc | assoc key action gtDisplayString ];
		column: 'Category' evaluated: [ :assoc | '' ] tags: [ :assoc | assoc value name ifNil: [ #() ] ifNotNil: [ { assoc value name }]];
		send: #key
]

{ #category : #'*GT-InspectorExtensions-Core' }
KMRepository >> gtInspectorMappingsIn: composite [
	<gtInspectorPresentationOrder: 40>
	composite table 
		title: 'Keys';
		display: [ 
			self categories values
				flatCollect: [ :category | 
					category allEntries keymaps collect: [ :key | key -> category ] ]
				 ];
		filterOn: [ :text :each | each gtDisplayString includesSubstring: text caseSensitive: false ];
		column: 'Shortcut' evaluated: [ :assoc | assoc key shortcut gtDisplayString ] width: 120;
		column: 'Action' evaluated: [ :assoc | assoc key action gtDisplayString ];
		column: 'Description' evaluated: [ :assoc | assoc key description ];
		column: 'Category' evaluated: [ :assoc | '' ] tags: [ :assoc | assoc value name ifNil: [ #() ] ifNotNil: [ { assoc value name }]];
		send: #key
]
