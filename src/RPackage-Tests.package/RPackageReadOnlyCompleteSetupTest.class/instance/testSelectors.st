test situation
testSelectors
	"self debug: #testSelectors"
	
	self assert: (p1 selectors) size = 2. 
	self assert: ((p1 selectors) includes: #methodDefinedInP1).
	self assert: ((p1 selectors) includes: #anotherMethodDefinedInP1). 

	self assert: p3 selectors size = 2.
	self assert: (p3 selectors includes: #methodDefinedInP3).
	self assert:  (p3 selectors includes: #classSideMethodDefinedInP3)	