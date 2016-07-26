*GT-InspectorExtensions-Core
exampleWithDifferentObjects
	<gtExample>
	<label: 'Example with different objects'>
	
	^ OrderedDictionary  new
			at: 'Time'   put: Time now;
			at: 'Color'  put: Color cyan;
			at: 'Date'   put: Date today;
			at: 'Morph'  put: HSVAColorSelectorMorph new;
			yourself.