public
copy: aPath toReference: destinationReference

	^self = destinationReference fileSystem
		ifTrue: [ self copy: aPath to: destinationReference resolve path ]
		ifFalse: [ self copy: aPath toRemote: destinationReference ]