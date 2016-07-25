testing
includesPackage: aPackage
	^self dependencies anySatisfy: [:each | each package = aPackage]