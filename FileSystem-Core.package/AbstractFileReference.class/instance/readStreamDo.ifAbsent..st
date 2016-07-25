streams
readStreamDo: doBlock ifAbsent: absentBlock
	^ self isFile 
		ifTrue: [ self readStreamDo: doBlock ]
		ifFalse: absentBlock