tests
testStartingSituation
	"self debug: #testStartingSituation"


.	self deny: (p5 includesClass: a1).
	self assert: (p4 includesClass: a1).
	self assert: (p4 includesClass: t1).
	self assert: (p4 definesOrExtendsClass: a1).
	self assert: (p5 includesClass: t2).