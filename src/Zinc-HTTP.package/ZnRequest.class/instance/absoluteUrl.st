accessing
absoluteUrl
	^ self host 
		ifNil: [ self url ]
		ifNotNil: [ :hostUrl | self url inContextOf: hostUrl ]