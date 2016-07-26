accessing
markIndex
	^ self markBlock ifNotNil: [:m | m stringIndex] ifNil: [ 1 ]
