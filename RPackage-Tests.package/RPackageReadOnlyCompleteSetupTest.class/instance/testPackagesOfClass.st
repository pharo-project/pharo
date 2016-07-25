test accessing
testPackagesOfClass
	"since a class can be extended by several packages, we want the complete list of packages that define or extend
	the class"
	"self debug: #testPackagesOfClass"
		
	| packages extending |
	packages := a2 packages.
	"a2 is extended by p1 and p3"
	extending := a2 extendingPackages.
	self assert: extending size = 2. 
	self assert: (extending includes: p1).
	self assert: (extending includes: p3).
	
	self assert: packages size = 3. 
	self assert: (packages includes: p1).
	self assert: (packages includes: p2).
	self assert: (packages includes: p3).
	
	
	
	
	
	
	