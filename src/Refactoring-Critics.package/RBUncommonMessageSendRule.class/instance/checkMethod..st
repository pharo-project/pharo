running
checkMethod: aMethod 
	aMethod messages do: [ :each |
		(each isEmpty or: [ (self isUnknownCapitalizedMessage: each) or: [ literalNames includes: each ] ]) ifTrue: [
			result 
				addMethod: aMethod;
				addSearchString: each ] ]