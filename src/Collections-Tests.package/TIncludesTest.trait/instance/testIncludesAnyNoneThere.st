tests - includes
testIncludesAnyNoneThere
	"self debug: #testIncludesAnyOfNoneThere'"
	
	self deny: (self nonEmpty includesAny: self empty).
	self deny: (self nonEmpty includesAny: { self elementNotIn. self anotherElementNotIn })