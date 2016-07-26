code generation (closures)
analyseTempsWithin: scopeBlock "<BlockNode>"  rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools "<Dictionary>"
	"N.B.  since assigment happens _after_ the value is evaluated the value is sent the message _first_."
	value analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools.
	variable beingAssignedToAnalyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools