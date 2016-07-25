accessing
position
	^ self atEnd 
		ifTrue: [ totalSize ]
		ifFalse: [ totalSize - chunk size + position ]