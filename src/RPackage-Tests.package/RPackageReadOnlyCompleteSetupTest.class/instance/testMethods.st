test situation
testMethods
	"self debug: #testMethods"
	
	| m1 m3 |
	m1 := p1 methods.
	self assert: m1 size = 3. 
	self assert: (m1 includes: a1>>#methodDefinedInP1).
	self assert: (m1 includes: a1>>#anotherMethodDefinedInP1). 
	self assert: (m1 includes: a2>>#methodDefinedInP1).
	m3 := p3 methods.
	self assert: m3 size = 2.
	self assert: (m3 includes: a2>>#methodDefinedInP3).
	self assert: (m3 includes: a2 class>>#classSideMethodDefinedInP3)