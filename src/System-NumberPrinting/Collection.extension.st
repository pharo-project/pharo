Extension { #name : 'Collection' }

{ #category : '*System-NumberPrinting' }
Collection >> displayStringOn: stream [
	self printNameOn: stream.
	stream
		space;
		nextPut: $[;
		nextPutAll: self size asStringWithCommas;
		nextPutAll: (' item' asPluralBasedOn: self size);
		nextPut: $];
		space.
	self size <= self collectionSizeThreshold
		ifTrue: [ self printElementsOn: stream ]
]
