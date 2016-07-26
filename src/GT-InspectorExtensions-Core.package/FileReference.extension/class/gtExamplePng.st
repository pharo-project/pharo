*GT-InspectorExtensions-Core
gtExamplePng
	<gtExample>
	^ (FileSystem memory workingDirectory / 'pharoicon.png')
		writeStreamDo: [ :stream | 
			PNGReadWriter
				putForm: (Smalltalk ui icons iconNamed: #pharoIcon)
				onStream: stream ];
		yourself