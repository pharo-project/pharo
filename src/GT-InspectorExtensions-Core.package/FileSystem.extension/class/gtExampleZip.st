*GT-InspectorExtensions-Core
gtExampleZip
	<gtExample>
	<label: 'Zip file system'>
	
	| zipFile zip |
	zipFile := FileSystem memory workingDirectory / 'test.zip'.
	zip := (FileSystem zip: zipFile) open.
	zip / 'sample.txt'
		writeStreamDo: [ :stream |
				stream nextPutAll: self comment ];
			yourself.
	^ zip