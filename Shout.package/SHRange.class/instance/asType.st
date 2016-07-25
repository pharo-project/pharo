tests
asType

	type == #symbol ifTrue: [ ^Symbol ].
	type == #character ifTrue: [ ^Character ].
	type == #string ifTrue: [ ^String ].
	type == #number ifTrue: [ ^Number ].
	type == #true ifTrue: [ ^True ].
	type == #false ifTrue: [ ^False ].
	type == #arrayEnd ifTrue: [ ^Array ].
	type == #byteArrayEnd ifTrue: [ ^ByteArray ].
	self isBlockEnd ifTrue: [ ^BlockClosure ].
	^nil