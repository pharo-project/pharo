generating
generateDataOn: aStream
	| items |
	aStream tab; nextPutAll: '<data>'; nextPut: Character lf.
	aStream tab; tab; nextPutAll: '<all name="all classes">'; nextPut: Character lf.
	self
		generateType: 'class' indent: 3
		total: (items := (packages flatCollect: [ :each | each classes ]) asSet) size
		actual: ((covered collect: [ :each | each actualClass theNonMetaClass ]) asSet
			count: [ :each | items includes: each ])
		on: aStream.
	self
		generateType: 'method' indent: 3
		total: (items := (packages flatCollect: [ :each | each methods ]) asSet) size 
		actual: (covered count: [ :each | items includes: each ])
		on: aStream.
	packages do: [ :each | self generatePackage: each on: aStream ].
	aStream tab; tab; nextPutAll: '</all>'; nextPut: Character lf.
	aStream tab; nextPutAll: '</data>'; nextPut: Character lf