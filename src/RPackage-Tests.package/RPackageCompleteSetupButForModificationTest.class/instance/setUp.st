running
setUp
	super setUp.
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.
	p3 := self createNewPackageNamed: self p3Name.
	
	a1 :=  self createNewClassNamed: #A1DefinedInP1 inPackage: p1.
	b1 := self createNewClassNamed: #B1DefinedInP1 inPackage: p1.
	a2 := self createNewClassNamed: #A2DefinedInP2 inPackage: p2.
	b2 := self createNewClassNamed: #B2DefinedInP2 inPackage: p2.
	a3 := self createNewClassNamed: #A3DefinedInP3 inPackage: p3.
	
	a1 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a1>>#methodDefinedInP1).
	a1 compileSilently: 'anotherMethodDefinedInP1 ^ #anotherMethodDefinedInP1'.
	p1 addMethod: (a1>>#anotherMethodDefinedInP1).
	
	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a2>>#methodDefinedInP1).
	
	a2 compileSilently: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	p2 addMethod: (a2>>#methodDefinedInP2).
	
	a2 compileSilently: 'methodDefinedInP3 ^ #methodDefinedInP3'.
	p3 addMethod: (a2>>#methodDefinedInP3).

	a2 class compileSilently: 'classSideMethodDefinedInP3 ^ #classSideMethodDefinedInP3'.
	p3 addMethod: (a2 class>>#classSideMethodDefinedInP3)

