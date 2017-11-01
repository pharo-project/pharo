Extension { #name : #KMDispatcher }

{ #category : #'*GT-InspectorExtensions-Core' }
KMDispatcher >> gtAllKeyCategoryPairs [

	^ ((self targets collect: #category)
			add: self directKeymaps;
			yourself)
			flatCollect: [ :category | 
				category allEntries keymaps collect: [ :key | key -> category ] ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
KMDispatcher >> gtInspectorMappingsIn: composite [
	<gtInspectorPresentationOrder: 40>
	composite table 
		title: 'Keys';
		display: [ 
			self gtAllKeyCategoryPairs ];
		filterOn: [ :text :each | each gtDisplayString includesSubstring: text caseSensitive: false ];
		column: 'Shortcut' evaluated: [ :assoc | assoc key shortcut gtDisplayString ] width: 120;
		column: 'Action' evaluated: [ :assoc | assoc key action gtDisplayString ];
		column: 'Description' evaluated: [ :assoc | assoc key description ];
		column: 'Category' evaluated: [ :assoc | '' ] tags: [ :assoc | assoc value name ifNil: [ #() ] ifNotNil: [ { assoc value name }]];
		send: #key
]
