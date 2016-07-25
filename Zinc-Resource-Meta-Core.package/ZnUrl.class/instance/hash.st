comparing
hash
	^ self hasPath
		ifTrue: [ segments inject: host hash into: [ :sum :each | sum bitXor: each hash ] ]
		ifFalse: [ host hash ]