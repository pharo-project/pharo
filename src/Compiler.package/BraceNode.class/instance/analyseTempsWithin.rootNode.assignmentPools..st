code generation (closures)
analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	elements do:
		[:node|
		node analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools]