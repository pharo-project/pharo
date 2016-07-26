tests - includes
testIncludesAnyAllThere
	"self debug: #testIncludesAnyOfAllThere'"
	self deny: (self nonEmpty includesAny: self empty).
	self assert: (self nonEmpty includesAny: { self nonEmpty anyOne }).
	self assert: (self nonEmpty includesAny: self nonEmpty).