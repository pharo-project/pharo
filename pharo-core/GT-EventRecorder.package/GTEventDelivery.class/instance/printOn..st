printing
printOn: aStream
	super printOn: aStream.
	aStream
		space;
		nextPut: $[;
		print: queue size;
		space;
		nextPutAll: ('item' asPluralBasedOn: queue size).
	self isActive
		ifTrue: [ 
			delay 
				ifNil: [ aStream nextPutAll: 'process active, but delay is nil!' ]
				ifNotNil: [ :aDelay |
					aStream
						nextPutAll: ', next cycle in ';
						print: (aDelay millisecondsToGo milliSeconds roundTo: 1 second);
						nextPutAll: ' sec' ] ]
		ifFalse: [ 
			aStream nextPutAll: ', inactive' ].
	aStream		
		nextPut: $]