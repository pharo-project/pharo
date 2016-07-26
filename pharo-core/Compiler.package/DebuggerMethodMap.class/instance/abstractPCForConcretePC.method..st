source mapping
abstractPCForConcretePC: concretePC method: aMethod
	"Answer the abstractPC matching concretePC."

	| abstractPC scanner client |
	abstractPC := 1.
	scanner := InstructionStream on: aMethod.
	client := InstructionClient new.
	[(scanner atEnd
	  or: [scanner pc >= concretePC]) ifTrue:
		[^abstractPC].
	 abstractPC := abstractPC + 1.
	 scanner interpretNextInstructionFor: client.
	 true] whileTrue