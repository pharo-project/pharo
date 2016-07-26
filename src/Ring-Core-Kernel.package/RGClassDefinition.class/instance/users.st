managing pool users
users
	"If the reciever is a SharedPool then retrieves its users"
	
	^self isPool
		ifTrue: [ self annotationNamed: self class usersKey ifAbsentPut: [ OrderedCollection new ] ]
		ifFalse:[ #() ]