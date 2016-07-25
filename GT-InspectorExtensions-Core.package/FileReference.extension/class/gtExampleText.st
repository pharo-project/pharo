*GT-InspectorExtensions-Core
gtExampleText
	<gtExample>
	^ FileSystem memory workingDirectory / 'sample.txt' 
		writeStreamDo: [ :stream |
			stream nextPutAll: self comment ];
		yourself