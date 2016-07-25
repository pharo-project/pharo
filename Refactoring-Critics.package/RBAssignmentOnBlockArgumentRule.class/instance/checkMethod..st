running
checkMethod: aMethod
	(RBFindBlockArgumentAssignment collectAssignments: aMethod ast)
		do: [ :item | 
			result addMethod: aMethod.
			result addSearchString: item ]