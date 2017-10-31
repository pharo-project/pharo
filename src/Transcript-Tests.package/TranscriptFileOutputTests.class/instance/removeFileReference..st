utility
removeFileReference: aFileReference

	aFileReference exists ifTrue: [aFileReference delete].