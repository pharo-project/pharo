test method addition removal
testMethodPackageResolution
	| a1Name p1 a1 |
	a1Name := #A2InPackageP2.
	p1 := self createNewPackageNamed: 'P1'.
	a1 := self createNewClassNamed: a1Name inPackage: p1.
	a1 compileSilently: 'method ^ #methodDefinedInP2'.
	a1 class compileSilently: 'method ^ #methodDefinedInP2'.
	
	p1 addMethod:  (a1>>#method).
	p1 addMethod:  (a1 class>>#method).
	
	self assert: (a1 >> #method) package == p1.
	self assert: (a1 class >> #method) package == p1.