tests
testCreation
	"self run: #testCreation" 
	"self debug: #testCreation"
		
	| bag |
	bag := Bag new.
	self assert: (bag size) = 0.
	self assert: (bag isEmpty).
	
