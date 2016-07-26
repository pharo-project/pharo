printing
printOn: aStream
	aStream
		nextPutAll: self label;
		nextPut: $:;
		space;
		nextPutAll: self explanation.
	self reasonCode ~~ #none
		ifTrue: [ 
			aStream
				space;
				nextPut: ${;
				space;
				nextPutAll: self reasonCode asString;
				space;
				nextPut: $} ].
	(self configurationClass ~~ nil or: [ self callSite ~~ nil ])
		ifTrue: [ 
			aStream
				space;
				nextPut: $[;
				space.
			self configurationClass ~~ nil
				ifTrue: [ 
					aStream
						nextPutAll: self configurationClass name asString;
						space ].
			self callSite ~~ nil
				ifTrue: [ 
					aStream
						nextPutAll: self callSite name asString;
						space ].
			aStream nextPut: $] ]