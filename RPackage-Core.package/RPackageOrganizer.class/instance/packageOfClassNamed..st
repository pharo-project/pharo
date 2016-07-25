package access from class
packageOfClassNamed: aName

	self flag: self. "shouldProbablyReturnUnunpackagedPackage instead of nil"
	^ classPackageMapping at: aName asSymbol ifAbsent: [nil]