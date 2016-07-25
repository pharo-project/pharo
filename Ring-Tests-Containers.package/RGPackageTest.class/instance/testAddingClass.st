testing
testAddingClass
	| newPackage newClass |
	
	newPackage:= RGPackage named: #'Collections-Sequenceable'.
	newClass:= RGClassDefinition named: #OrderedCollection.
	newPackage addClass: newClass.
	
	self assert: (newPackage hasClasses).
	self assert: (newClass package == newPackage).
	self assert: ((newPackage classNamed: #OrderedCollection) == newClass)