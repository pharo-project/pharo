printing
printOn: aStream
	super printOn: aStream.
	aStream
		nextPutAll: '(';
		print: millisecondDelayDuration;
		nextPutAll: ' msecs'.
	beingWaitedOn
		ifTrue: [ 
			aStream
				nextPutAll: '; ';
				print: self millisecondsToGo;
				nextPutAll: ' msecs remaining' ].
	aStream nextPutAll: ')'.