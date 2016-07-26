testing-basic
testOriginSelectorOf
	
	"regular method "
	self assert: (MOPTestClassC traitComposition originSelectorOf: #c) equals: #c.	
	"non aliased trait method "
	self assert: (MOPTestClassD traitComposition originSelectorOf: #c2) equals: #c2.	
	"aliased method "
	self assert: (MOPTestClassD traitComposition originSelectorOf: #c3) equals: #c2.