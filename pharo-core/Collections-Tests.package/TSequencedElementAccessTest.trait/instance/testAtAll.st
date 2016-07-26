tests - element accessing
testAtAll
	"self debug: #testAtAll"
	"	self flag: #theCollectionshouldbe102030intheFixture.
	
	self assert: (self accessCollection atAll: #(2 1)) first = self accessCollection second.
	self assert: (self accessCollection atAll: #(2)) first = self accessCollection second."
	| result |
	result := self moreThan4Elements atAll: #(2 1 2 ).
	self assert: (result at: 1) = (self moreThan4Elements at: 2).
	self assert: (result at: 2) = (self moreThan4Elements at: 1).
	self assert: (result at: 3) = (self moreThan4Elements at: 2).
	self assert: (self moreThan4Elements atAll: #()) = self moreThan4Elements species new