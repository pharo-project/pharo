control flow instructions
decodeLoop: loop nextLoops: loops
	loop isConditionalLoop
		ifTrue: [ self decodeConditionalLoop: loop nextLoops: loops ]
		ifFalse: [ self decodeRepeatLoop: loop nextLoops: loops ]