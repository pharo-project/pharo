printing
storeOn: aStream
	| sizeField |
	(sizeField := self sizeField) ~= 0 ifTrue:
		[aStream nextPut: $(].
	aStream nextPut: $(; print: self class.
	sizeField = 0
		ifTrue:
			[aStream nextPutAll: ' forPointer: '; nextPutAll: self addressField storeStringHex]
		ifFalse:
			[aStream
				nextPutAll: (sizeField < 0 ifTrue: [' newC: '] ifFalse: [' new: ']);
				print: (sizeField := sizeField abs); nextPut: $).
			 1 to: sizeField by: 4 do:
				[:i| | v |
				(v := self unsignedLongAt: i) ~= 0 ifTrue:
					[aStream
						crtab;
						nextPutAll: 'unsignedLongAt: '; print: i;
						nextPutAll: ' put: '; print: v; nextPut: $;]].
			 aStream crtab; nextPutAll: #yourself].
	aStream nextPut: $)

	"{	(Alien forPointer: 0) storeString.
		(Alien new: 8) storeString.
		(Alien newC: 8) storeString }"