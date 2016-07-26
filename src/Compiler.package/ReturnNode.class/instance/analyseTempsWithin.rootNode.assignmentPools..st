code generation (closures)
analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	"Note we could do this:
		scopeBlock ~~ rootNode block ifTrue:
			[scopeBlock noteNonLocalReturn].
	 and pass up the flag in <BlockNode>>>analyseTempsWithin:rootNode:
	 which may be fast but will also give less information the debugger.
	 For now we consider clean blocks a premature optimization."
	self flag: 'consider clean blocks'.
	expr analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools