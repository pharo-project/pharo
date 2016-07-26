doing
on: aSymbol do: aBlock ifAbsent: absentBlock
	|state|
	state := (aSymbol cull: self)
		ifTrue: [ self ]
		ifFalse: absentBlock.
	aBlock cull: state.
	^ state