printing
printOn: aStream
	aStream nextPutAll: 'WP'.
	tagname isAllSeparators 
		ifFalse: [ 
			aStream nextPutAll: ' '.
			aStream nextPutAll: tagname ]. 
	aStream nextPutAll: ': '.
	aStream nextPutAll: (self printLastValue).