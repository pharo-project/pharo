*metacello-mc
compare: aLoadableReference using: aComparisonOperator
	"Compare versions using <aComparisonOperator>. package names #= then compare based upon version number
	Branches and Author names are used in the case of a version number tie, because we need to avoid seesaw loading."
		
	self packageName = aLoadableReference packageName
		ifFalse: [ ^false ].
	self versionNumber = aLoadableReference versionNumber
		ifFalse: [ ^ self versionNumber perform: aComparisonOperator with: aLoadableReference versionNumber ].
	self branch = aLoadableReference branch 
		ifFalse: [ ^ self branch perform: aComparisonOperator with: aLoadableReference branch ].
	^ self author perform: aComparisonOperator with: aLoadableReference author