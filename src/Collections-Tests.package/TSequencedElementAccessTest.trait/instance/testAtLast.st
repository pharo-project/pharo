tests - element accessing
testAtLast
	"self debug: #testAtLast"
	| index |
	self assert: (self moreThan4Elements atLast: 1) = self moreThan4Elements last.
	"tmp:=1.
	self do:
		[:each | 
		each =self elementInForIndexAccessing 
			ifTrue:[index:=tmp].
		tmp:=tmp+1]."
	index := self moreThan4Elements indexOf: self elementInForElementAccessing.
	self assert: (self moreThan4Elements atLast: index) = (self moreThan4Elements at: self moreThan4Elements size - index + 1)