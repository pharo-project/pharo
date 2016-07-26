printing
configMethodCascadeOn: aStream lastCascade: lastCascade

	aStream nextPutAll: 'repository: ', self description printString.
	(self username isEmpty not or: [ self password isEmpty not ])
		ifTrue: [ aStream nextPutAll: ' username: ', self username printString, ' password: ', self password printString ].
	lastCascade ifFalse: [ aStream nextPut: $;; cr ].