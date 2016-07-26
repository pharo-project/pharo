running
run: aPDPackageRelation
	aPDPackageRelation packages
		do: [ :package | 
			package isTarjanUndefined
				ifTrue: [ self traverse: package ] ]
		displayingProgress: 'Finding SCCs'