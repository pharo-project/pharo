generating
generatePackage: aPackage on: aStream
	| items |
	aStream tab: 3; nextPutAll: '<package name="'; nextPutAll: (self encode: (aPackage packageName copyReplaceAll: '-' with: '.')); nextPutAll: '">'; nextPut: Character lf.
	self
		generateType: 'class' indent: 4
		total: (items := aPackage classes asSet) size
		actual: ((covered collect: [ :each | each actualClass theNonMetaClass ]) asSet
			count: [ :each | items includes: each ])
		on: aStream.
	self
		generateType: 'method' indent: 4
		total: (items := aPackage methods asSet) size
		actual: (covered count: [ :each | items includes: each ])
		on: aStream.
	aPackage classes 
		do: [ :class | self generatePackage: aPackage class: class on: aStream ].
	aStream tab: 3; nextPutAll: '</package>'; nextPut: Character lf