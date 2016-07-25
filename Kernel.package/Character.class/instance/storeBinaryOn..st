printing
storeBinaryOn: arg1
	self asInteger < 256
		ifTrue: [ arg1 basicNextPut: self asInteger ]
		ifFalse: [ arg1 nextInt32Put: self asInteger ]