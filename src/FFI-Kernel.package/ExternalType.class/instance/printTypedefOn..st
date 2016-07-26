printing
printTypedefOn: s
	s nextPutAll: 'typedef '.
	(compiledSpec first bitClear: FFIStructSizeMask) = FFIFlagStructure
		ifTrue:
			[| next |
			 next := 2.
			 s nextPutAll: 'struct {'.
			 referentClass fields withIndexDo:
				[:tuple :i|
				s cr.
				next := self printStructureFieldStartingAt: next
							withName: tuple first
							inClass: referentClass
							on: s
							indent: 1.
				s nextPut: $;].
			 s cr; nextPutAll: '} '.
			 self assert: (next - 1 = compiledSpec size or: [(compiledSpec at: next) = FFIFlagStructure])]
		ifFalse:
			[self printAtomicType: compiledSpec first on: s].
	s nextPutAll: (referentClass ifNotNil: [referentClass name] ifNil: ['foo'])