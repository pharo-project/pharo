api
decode
	
	| end |
	symbolicBytecodes := OrderedCollection new.
	scanner := InstructionStream on: method.
	end := method endPC.
	oldPC := scanner pc.
	[scanner pc <= end] whileTrue:
		[scanner interpretNextInstructionFor: self].
	^symbolicBytecodes