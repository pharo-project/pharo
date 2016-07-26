accessing
sourcePointer: ptr

	self clear.
	data := ptr.
	"see if we can encode pointer using 4-byte trailer"
	kind := (ptr between: 16r1000000 and: 16r4FFFFFF) 
		ifTrue: [ #SourcePointer ] ifFalse: [ #VarLengthSourcePointer ].
	