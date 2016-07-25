writing
writeObject: anObject do: block
	(jsonMode and: [ anObject class ~= STON listClass and: [ anObject class ~= STON mapClass ] ])
		ifTrue: [ self error: 'wrong object class for JSON mode' ].
	self with: anObject do: [
		writeStream nextPutAll: anObject class stonName.
		self prettyPrintSpace.
		block value ]