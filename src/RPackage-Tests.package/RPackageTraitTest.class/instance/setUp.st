running
setUp

	super setUp.
	
	p4 := self createNewPackageNamed: self p4Name.  
	p5 := self createNewPackageNamed: self p5Name. 
	p6 := self createNewPackageNamed: self p6Name.  
	
	a1 :=  self createNewClassNamed: #A1DefinedInP1 inPackage: p4.
	
	"a1 defines two normal = local methods"
	a1 compileSilently: 'localMethodDefinedInP1 ^ #methodDefinedInP1'.
	p4 addMethod: (a1>>#localMethodDefinedInP1).
	a1 compileSilently: 'anotherLocalMethodDefinedInP1 ^ #anotherMethodDefinedInP1'.
	p4 addMethod: (a1>>#anotherLocalMethodDefinedInP1).
	
	t1 := self createNewTraitNamed: #TraitInPackageP4 inPackage: p4.
	t1 compileSilently: 'traitMethodDefinedInP4 ^ #traitMethodDefinedInP4'.
	p4 addMethod: (t1>>#traitMethodDefinedInP4).
	
	"P6 defines a new method extension on T1 (packaged in p4)"
	t1 compileSilently: 'traitMethodExtendedFromP6 ^ #traitMethodExtendedFromP6' classified: ('*',self p6Name). 
	p6 addMethod: (t1>>#traitMethodExtendedFromP6).
	
	t2 := self createNewTraitNamed: #TraitInPackageP5 inPackage: p5.
	t2 compileSilently: 'traitMethodDefinedInP5 ^ #traitMethodDefinedInP5'.
	
	p5 addMethod: (t2 >> #traitMethodDefinedInP5).  
	
	"Here P4 extends T2 from P5 with a new method"
	t2 compileSilently: 'traitMethodExtendedFromP4 ^ #traitMethodExtendedFromP4' classified: ('*',self p4Name). 
	p4 addMethod: (t2 >> #traitMethodExtendedFromP4).
	
	a1 setTraitComposition: (t1 + t2) asTraitComposition.
	
	"a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a2>>#methodDefinedInP1).
	
	a2 compileSilently: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	p2 addMethod: (a2>>#methodDefinedInP2).
	
	a2 compileSilently: 'methodDefinedInP3 ^ #methodDefinedInP3'.
	p3 addMethod: (a2>>#methodDefinedInP3).

	a2 class compileSilently: 'classSideMethodDefinedInP3 ^ #classSideMethodDefinedInP3'.
	p3 addMethod: (a2 class>>#classSideMethodDefinedInP3)."
	