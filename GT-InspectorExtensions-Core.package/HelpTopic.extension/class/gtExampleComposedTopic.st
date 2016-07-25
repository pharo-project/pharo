*GT-InspectorExtensions-Core
gtExampleComposedTopic
	<gtExample>
	|rootTopic|
	
	rootTopic := HelpTopic new
		title: 'ExampleTopic';
		contents: 'I am an example topic'.
	rootTopic addSubtopic: (HelpTopic new
		title: 'Subtopic1';
		contents: 'I am an subtopic topic').
	rootTopic addSubtopic: (HelpTopic new
		title: 'Subtopic2').
		
	^ rootTopic
		