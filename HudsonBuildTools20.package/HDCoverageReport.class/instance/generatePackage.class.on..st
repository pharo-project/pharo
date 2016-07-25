generating
generatePackage: aPackage class: aClass on: aStream
	| items |
	aStream tab: 4; nextPutAll: '<class name="'; nextPutAll: (self encode: aClass name); nextPutAll: '">'; nextPut: Character lf.
	self
		generateType: 'class' indent: 5
		total: 1
		actual: ((covered anySatisfy: [ :each | each actualClass theNonMetaClass = aClass ])
			ifTrue: [ 1 ] ifFalse: [ 0 ])
		on: aStream.
	self
		generateType: 'method' indent: 5
		total: (items := aPackage coreMethodsForClass: aClass) size
		actual: (covered count: [ :each | items includes: each ])
		on: aStream.
	items do: [ :each | self generatePackage: each method: each on: aStream ].	
	aStream tab: 4; nextPutAll: '</class>'; nextPut: Character lf