tests
testMethodPackageOfRemovedClass
	"self debug: #testMethodPackageOfRemovedClass"
	
	| pack method foo | 
	pack := self createNewPackageNamed: 'P1'.
	foo := self createNewClassNamed: #FooForTest2 inPackage: pack.
	foo compileSilently: 'bar ^42'.
	method := foo>>#bar.
	foo removeFromSystem.
	self deny: (pack includesClassNamed: #FooForTest2 )
	
	