printing
printElementsOn: aStream 
	aStream nextPut: $(.
	self size > 100
		ifTrue: [aStream nextPutAll: 'size '.
			self size printOn: aStream]
		ifFalse: [self keysSortedSafely
				do: [:key | aStream print: key;
						 nextPutAll: '->';				
						 print: (self at: key);
						 space]].
	aStream nextPut: $)