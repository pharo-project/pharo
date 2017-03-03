printing
printOn: aStream

	aStream
		nextPutAll: self label;
		nextPut: $:;
		space;
		nextPutAll: self explanation.
		
	self reasonCode ~~ #none ifTrue: [ 
		aStream space;
			nextPut: ${;
			space;
			<< self reasonCode;
			space;
			nextPut: $} 
	].

	(self configurationClass isNil and: [ self callSite isNil ]) ifTrue: [ ^ self ].
	
	aStream space; nextPut: $[; space.
		
	self configurationClass ifNotNil: [ 
		aStream nextPutAll: (self configurationClass name ifNil: ['']); space 
	].

	self callSite ifNotNil: [ 
		aStream nextPutAll: self callSite; space 
	].

	aStream nextPut: $]  