test starting situation
testStartingSituation
	"self debug: #testStartingSituation"
	
	self deny: (p2 includesClass: b1).
	self assert: (p2 includesClass: b2).
	"a locally defined class not extended by other packages"
	
	self assert: (p2 includesClass: a2).  
	"a locally defined class extended by other packages"
	
	self assert: (p1 definesOrExtendsClass: a2).
	self deny: (p1 includesClass: a2).