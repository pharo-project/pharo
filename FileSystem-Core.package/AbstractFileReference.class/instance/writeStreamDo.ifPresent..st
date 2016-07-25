streams
writeStreamDo: doBlock ifPresent: presentBlock
	^ self isFile
		ifTrue: presentBlock
		ifFalse: [ self writeStreamDo: doBlock ]