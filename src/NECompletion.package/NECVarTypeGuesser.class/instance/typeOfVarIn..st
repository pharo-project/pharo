bytecode decoding
typeOfVarIn: aMethod 
	"Answer whether the receiver references an instance variable."
	| scanner end type infos |
	scanner := InstructionStream on: aMethod.
	end := scanner method endPC.
	currentMethod := aMethod.
	infos := OrderedCollection new.
	[ scanner pc <= end ] whileTrue: 
		[ (self interpretNextInstructionUsing: scanner) ifTrue: 
			[ type := self computeVarType.
			type ifNotNil: [ infos add: type ] ] ].
	^ infos