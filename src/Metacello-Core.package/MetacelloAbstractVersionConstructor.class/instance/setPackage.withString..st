api callbacks
setPackage: aString withString: aFile

	| spec |
	spec := 
		(self project packageSpec)
			name: aString;
			file: aFile;
			yourself.
	self root packages merge: spec.