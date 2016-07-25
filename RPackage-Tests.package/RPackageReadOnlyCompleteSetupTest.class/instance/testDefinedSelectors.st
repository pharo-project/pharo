test situation
testDefinedSelectors
	"self debug: #testDefinedSelectors"
	
	self assert: a1 definedSelectors size = 2. 
	self assert: ((a1 definedSelectors) includes: #methodDefinedInP1).
	self assert: ((a1 definedSelectors) includes: #anotherMethodDefinedInP1). 

	self assert: a2 definedSelectors size = 1.
	self assert: (a2 definedSelectors includes: #methodDefinedInP2).