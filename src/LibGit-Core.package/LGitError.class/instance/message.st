accessing
message
	^ self isNull
		ifFalse: [ self prim_message readString ]
		ifTrue: [ 'no error message set by libgit2' ]