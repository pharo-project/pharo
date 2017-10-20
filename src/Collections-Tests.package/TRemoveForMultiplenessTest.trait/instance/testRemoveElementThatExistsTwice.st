tests - remove
testRemoveElementThatExistsTwice
	"self debug: #testRemoveElementThatDoesExistsTwice"

	| size |
	size := self nonEmpty size.
	self assert: (self nonEmpty includes: self elementTwiceIn).
	self nonEmpty remove: self elementTwiceIn.
	self assert: size - 1 = self nonEmpty size.
	
	self assert: (self nonEmpty includes: self elementTwiceIn).
	self nonEmpty remove: self elementTwiceIn.
	self assert: size - 2 = self nonEmpty size