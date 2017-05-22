accessing
type
	^ self isNull
		ifFalse: [ self prim_klass ]
		ifTrue: [ nil ]