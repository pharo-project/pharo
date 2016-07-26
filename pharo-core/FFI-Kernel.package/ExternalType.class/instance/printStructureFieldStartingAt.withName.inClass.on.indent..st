printing
printStructureFieldStartingAt: initialSpecIndex withName: name inClass: structureClass on: aStream indent: indent
	"Print the structure's field starting at initialSpecIndex and answer the index in compiledSpec of the subsequent type."
	| spec subStructureClassBinding |
	aStream tab: indent.
	spec := compiledSpec at: initialSpecIndex.
	(spec anyMask: FFIFlagAtomic) ifTrue:
		[self printAtomicType: spec on: aStream.
		 aStream nextPutAll: (name ifNotNil: [name] ifNil: ['foo']).
		 ^initialSpecIndex + 1].
	 subStructureClassBinding := (structureClass >> name) literals detect:
									[:l| l isVariableBinding and: [l value inheritsFrom: ExternalStructure]].
	(spec bitClear: FFIStructSizeMask) = FFIFlagStructure ifTrue:
		[| next |
		 next := initialSpecIndex + 1.
		 aStream nextPutAll: 'struct {'.
		 subStructureClassBinding value fields withIndexDo:
			[:tuple :i|
			aStream cr.
			next := self printStructureFieldStartingAt: next
						withName: tuple first
						inClass: subStructureClassBinding value
						on: aStream
						indent: indent + 1.
			aStream nextPut: $;].
		 aStream crtab: indent; nextPut: $}.
		 name ifNotNil: [aStream space; nextPutAll: name].
		 self assert: (next - 1 = compiledSpec size or: [(compiledSpec at: next) = FFIFlagStructure]).
		 ^next <= compiledSpec size ifTrue: [next + 1] ifFalse: [next]].
	self assert: (spec anyMask: FFIFlagPointer).
	aStream nextPutAll: 'struct '; nextPutAll: subStructureClassBinding value name; nextPutAll: ' *'; nextPutAll: name.
	^initialSpecIndex + 1