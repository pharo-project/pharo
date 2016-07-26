writing
writeNull
	jsonMode
		ifTrue: [ writeStream nextPutAll: 'null' ]
		ifFalse: [ writeStream print: nil ]