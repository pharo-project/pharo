printing
storeOn: aStream 
	aStream nextPut: $(;
	 	store: start;
	 	nextPutAll: ' to: ';
	 	store: stop.
	step ~= 1 ifTrue: [aStream nextPutAll: ' by: '; store: step].
	aStream nextPut: $)