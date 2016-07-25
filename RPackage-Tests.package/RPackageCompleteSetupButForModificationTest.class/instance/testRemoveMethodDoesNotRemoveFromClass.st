test addition
testRemoveMethodDoesNotRemoveFromClass
	"we remove a method so it is not in the package but also not from the class anymore"
	"self debug: #testRemoveMethodDoesNotRemoveFromClass"
	
	p1 removeMethod:  a1 >> #methodDefinedInP1.
	self deny: (p1 includesSelector: #methodDefinedInP1 ofClass: a1).
	self assert: (a1 selectors includes: #methodDefinedInP1)