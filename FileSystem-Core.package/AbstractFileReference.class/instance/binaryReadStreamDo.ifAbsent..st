streams-compatibility
binaryReadStreamDo: doBlock ifAbsent: absentBlock
	^ self isFile 
		ifTrue: [ self binaryReadStreamDo: doBlock ]
		ifFalse: absentBlock