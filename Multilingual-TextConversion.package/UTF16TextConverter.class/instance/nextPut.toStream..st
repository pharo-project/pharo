conversion
nextPut: aCharacter toStream: aStream

	| v low high |
	
	self checkBOMOn: aStream.

	v := aCharacter charCode.
	v > 16rFFFF ifFalse: [
		self next16BitValue: v toStream: aStream.
		^ self.
	] ifTrue: [
		v := v - 16r10000.
		low := (v \\ 16r400) + 16rDC00.
		high := (v // 16r400) + 16rD800.
		self next16BitValue: high toStream: aStream.
		self next16BitValue: low toStream: aStream.
	]