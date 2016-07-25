running
checkPackage: aPackage
	(self basicCheck: aPackage) ifTrue: [ 
		result addPackage: aPackage ]