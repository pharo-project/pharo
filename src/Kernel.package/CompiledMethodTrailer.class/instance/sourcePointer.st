accessing
sourcePointer

	kind == #NativeCodeTrailer ifTrue: [ ^ data at: 3].
	^ (kind == #SourcePointer or: [ kind == #VarLengthSourcePointer ] )
		ifTrue: [ data ]
		ifFalse: [ 0 ]
