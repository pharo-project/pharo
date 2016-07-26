streams
writeStreamIfPresent: presentBlock
	^ self isFile 
		ifTrue: presentBlock
		ifFalse: [ self writeStream ]