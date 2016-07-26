*GT-InspectorExtensions-Core
gtExampleGif
	<gtExample>
	^ (FileSystem memory workingDirectory / 'pharoicon.gif')
		writeStreamDo: [ :stream | 
			GIFReadWriter
				putForm: (Smalltalk ui icons iconNamed: #pharoIcon)
				onStream: stream ];
		yourself