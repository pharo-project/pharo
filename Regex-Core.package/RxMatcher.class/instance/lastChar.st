accessing
lastChar
	^ stream position = 0
		ifFalse: [ stream skip: -1; next ]