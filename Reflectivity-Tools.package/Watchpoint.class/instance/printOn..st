printing
printOn: aStream
	aStream nextPutAll: 'Watchpoint: '.
	tagname isAllSeparators 
		ifTrue: [ aStream nextPutAll: 'Unnamed' ]
		ifFalse: [ aStream nextPutAll: tagname ]  