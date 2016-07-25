printing
printOn: aStream
	| name |
	(name := self name).
	name = #unnamed
		ifFalse: [ 
			^ aStream
				nextPutAll: 'Color ';
				nextPutAll: name ].
	self storeOn: aStream