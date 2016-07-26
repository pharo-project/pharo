test addition
testBasicRemoveClass
	"we remove a class we check that it is not in the package anymore"
	"self debug: #testBasicRemoveClass"	

	|  size |
	size := p1 definedClasses size. 
	self assert: size = 2.
	self assert: (p1 includesClass: a1).
	self assert: (p1 includesClass: a1 class).
	p1 removeClassDefinition: a1.
	self assert: p1 definedClasses size = (size - 1).
	self assert: (p1 includesClass: b1).
	self assert: (p1 includesClass: b1 class).
	self deny: (p1 includesClass: a1).
	self deny: (p1 includesClass: a1 class).
	p1 removeClassDefinition: b1.
	self assert: p1 definedClasses size = (size - 2).
	self deny: (p1 includesClass: b1).
	self deny: (p1 includesClass: b1 class).

