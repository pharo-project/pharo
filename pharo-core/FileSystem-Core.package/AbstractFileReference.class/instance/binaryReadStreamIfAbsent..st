streams-compatibility
binaryReadStreamIfAbsent: absentBlock
	^ self isFile
		ifTrue: [ self binaryReadStream ]
		ifFalse: absentBlock