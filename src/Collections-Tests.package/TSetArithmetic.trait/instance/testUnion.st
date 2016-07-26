tests - set arithmetic
testUnion
	"self debug: #testUnionOfEmpties"
	
	| union |
	union := self empty union: self nonEmpty.
	self containsAll: union of: self empty andOf: self nonEmpty.
	union := self nonEmpty union: self empty.
	self containsAll: union of: self empty andOf: self nonEmpty.
	union := self collection union: self nonEmpty.
	self containsAll: union of: self collection andOf: self nonEmpty.