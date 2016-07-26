printing
printReasonOn: aStream
	aStream nextPutAll: dependencies size asString; nextPutAll: ' dependencies: '.
	dependencies
		do: [ :each | each printReasonOn: aStream ]
		separatedBy: [ aStream nextPutAll: ', ' ]