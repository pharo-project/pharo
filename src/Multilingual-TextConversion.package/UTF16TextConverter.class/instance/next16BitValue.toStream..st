private
next16BitValue: value toStream: aStream

	| v1 v2 |
	v1 := (value >> 8) bitAnd: 16rFF.
	v2 := value bitAnd: 16rFF.

	self useLittleEndian ifTrue: [
		aStream basicNextPut: (Character value: v2).
		aStream basicNextPut: (Character value: v1).
	] ifFalse: [
		aStream basicNextPut: (Character value: v1).
		aStream basicNextPut: (Character value: v2).
	].
