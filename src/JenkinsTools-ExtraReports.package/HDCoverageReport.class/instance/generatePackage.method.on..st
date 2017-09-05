generating
generatePackage: aPackage method: aReference on: aStream
	| items |
	aStream tab: 5; nextPutAll: '<method name="'; nextPutAll: (self encode: aReference selector); nextPutAll: '">'; nextPut: Character lf.
	self
		generateType: 'method' indent: 6
		total: 1
		actual: ((covered includes: aReference) ifTrue: [ 1 ] ifFalse: [ 0 ])
		on: aStream.
	aStream tab: 5; nextPutAll: '</method>'; nextPut: Character lf