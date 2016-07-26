tests - compare
testComparisonWhenPrimitiveFails
	"This is related to http://bugs.squeak.org/view.php?id=7361"

	self deny: 0.5 < (1/4).
	self deny: 0.5 < (1/2).
	self assert: 0.5 < (3/4).
	
	self deny: 0.5 <= (1/4).
	self assert: 0.5 <= (1/2).
	self assert: 0.5 <= (3/4).
	
	self assert: 0.5 > (1/4).
	self deny: 0.5 > (1/2).
	self deny: 0.5 > (3/4).
	
	self assert: 0.5 >= (1/4).
	self assert: 0.5 >= (1/2).
	self deny: 0.5 >= (3/4).
	
	self deny: 0.5 = (1/4).
	self assert: 0.5 = (1/2).
	self deny: 0.5 = (3/4).
	
	self assert: 0.5 ~= (1/4).
	self deny: 0.5 ~= (1/2).
	self assert: 0.5 ~= (3/4).