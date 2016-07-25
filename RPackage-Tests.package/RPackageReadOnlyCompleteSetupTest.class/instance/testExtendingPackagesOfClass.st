test accessing
testExtendingPackagesOfClass
	"since a class can be extended by several packages, we want the list of packages that extend
	the class"
	
	"self debug: #testExtendingPackagesOfClass"
		
	| packages |
	packages := a2 extendingPackages.
	"a2 is extended by p1 and p3"
	self assert: packages size = 2. 
	self assert: (packages includes: p1).
	self deny: (packages includes: p2).
	self assert: (packages includes: p3).
	
	packages := a1 extendingPackages.
	self assert: packages isEmpty. 