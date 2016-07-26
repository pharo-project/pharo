tests - includes
testIncludesElementIsNotThere
	"self debug: #testIncludesElementIsNotThere"
	
	self deny: (self nonEmpty includes: self elementNotIn).
	self assert: (self nonEmpty includes: self nonEmpty anyOne).
	self deny: (self empty includes: self elementNotIn)