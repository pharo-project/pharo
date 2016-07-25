*GT-InspectorExtensions-Core
gtExampleOneElementDictionaryFrom: anEmptyDictionary
	<gtExample>
	<label: 'One element dictionary'>
	<depends: #gtExampleEmptyDictionary>
	
	anEmptyDictionary at: 4 put: 2.
	^ anEmptyDictionary