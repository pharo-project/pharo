test addition
testRemoveMethod
	"self debug: #testRemoveMethod"
	
	p1 removeMethod: a1 >> #methodDefinedInP1.
	self deny: (p1 includesSelector: #methodDefinedInP1 ofClass: a1).
	