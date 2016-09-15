instance creation
newNamed: aName inDirectory: baseDirectoryFileReference

	| newInstance |
	newInstance := self named: aName inDirectory: baseDirectoryFileReference.
	
	newInstance fileReference exists ifTrue: [ 
		FileExists signalWith: newInstance fileReference ].
	
	^ newInstance