accessing
directoryPathString
	^ self repository subdirectory isEmptyOrNil 
		ifTrue: [ self directoryName ]
		ifFalse: [ self repository subdirectory, '/',  self directoryName ].
