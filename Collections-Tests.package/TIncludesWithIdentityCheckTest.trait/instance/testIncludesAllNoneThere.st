tests - includes
testIncludesAllNoneThere
	"self debug: #testIncludesAllOfNoneThere'"
	self deny: (self empty includesAll: self nonEmpty ).
	self deny: (self nonEmpty includesAll: { self elementNotIn. self anotherElementNotIn })