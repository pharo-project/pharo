stdio
standardIOStreamNamed: moniker forWrite: forWrite
	"Create if necessary and store default stdin, stdout and other files based on the their names"
	
	| handle |
	
	self flag: #todo. "This is an ugly hack, while waiting for a real fix for windows. There  several problems with this approach, but it allow us to run tests, etc."
	Smalltalk os isWin32 
		ifTrue: [ 
			[ ^ (File named: moniker asString) writeStream ]
			on: CannotDeleteFileException do: [
				"HACK: if the image is opened a second time windows barks about the already opened locked file"
				^ (File named: moniker asString, '_', (Random new nextInt: SmallInteger maxVal) asString) readStream ]]. 	

	handle := self stdioHandles at: (#(stdin stdout stderr) identityIndexOf: moniker).
	handle ifNil: [ self error: moniker, ' is unavailable' ].
	^ StdioStream handle: handle file: (File named: moniker) forWrite: forWrite.