converting
asPointerType: pointerSize
	"convert the receiver into a pointer type"
	| type |
	type := self asPointerType.
	^type pointerSize = pointerSize
		ifTrue: [type]
		ifFalse:
			[type copy pointerSize: pointerSize; yourself]