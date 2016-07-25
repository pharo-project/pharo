*GT-InspectorExtensions-Core
gtExampleRemoveElementFrom: anEmptyDictionary
	<gtExample>
	<label: 'Remove element'>
	<depends: #gtExampleOneElementDictionaryFrom:>
	
	anEmptyDictionary removeKey: 4.
	^ anEmptyDictionary