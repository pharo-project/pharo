testing
testChanges
	"Test the most important features to ensure that
	general functionality of class traits are working."

	"self run: #testChanges"

	| classTrait |
	classTrait := self t1 classTrait.
	self deny: (self t5 classSide includesSelector: #m1ClassSide).
	classTrait compile: 'm1ClassSide ^17' classified: 'mycategory'.	"local selectors"
	self assert: (classTrait includesLocalSelector: #m1ClassSide).
	self deny: (classTrait includesLocalSelector: #otherSelector).	"propagation"
	self assert: (self t5 classSide methodDict includesKey: #m1ClassSide).
	self assert: (self c2 class methodDict includesKey: #m1ClassSide).
	self c2 m1ClassSide.
	self assert: self c2 m1ClassSide equals: 17.	"category"
	self assert: (self c2 class organization categoryOfElement: #m1ClassSide) equals: 'mycategory'.	"conflicts"
	self t2 classSide compile: 'm1ClassSide' classified: 'mycategory'.
	self assert: (self c2 class methodDict includesKey: #m1ClassSide).
	self deny: (self c2 class includesLocalSelector: #m1ClassSide).
	self should: [ self c2 m1ClassSide ] raise: Error.	"conflict category"
	self assert: (self c2 class organization categoryOfElement: #m1ClassSide) equals: #mycategory