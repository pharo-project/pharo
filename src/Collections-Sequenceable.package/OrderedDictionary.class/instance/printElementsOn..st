printing
printElementsOn: aStream
	aStream nextPut: $(.
	self size > 100
		ifTrue: [
			aStream nextPutAll: 'size '.
			self size printOn: aStream]
		ifFalse: [
			self associations withIndexDo: [:each :i |
				aStream
					print: each key;
					nextPutAll: '->';
					print: each value.
				(i < self size)
					ifTrue: [aStream space]]].
	aStream nextPut: $).