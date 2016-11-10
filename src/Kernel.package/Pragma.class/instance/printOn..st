printing
printOn: aStream
	aStream nextPut: $<.
	self selector precedence = 1
		ifTrue: [ aStream nextPutAll: self selector ]
		ifFalse: [ self selector keywords
				with: self arguments
				do: [ :key :arg | 
					aStream
						nextPutAll: key;
						space;
						print: arg;
						space ].
			aStream skip: -1 ].
	aStream nextPut: $>