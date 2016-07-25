printing
printElementsOn: aStream
	aStream nextPut: $(.
	self size > 100
		ifTrue: [
			aStream nextPutAll: 'size '.
			self size printOn: aStream]
		ifFalse: [
			self associations doWithIndex: [:association :i |
				aStream
					print: association key;
					nextPutAll: '->';
					print: association value.
				(i < self size)
					ifTrue: [aStream space]]].
	aStream nextPut: $).