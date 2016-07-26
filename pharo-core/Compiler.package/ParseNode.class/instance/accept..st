visiting
accept: aVisitor
	"Accept a visitor by double-dispatching to a type-specific method on the visitor, e.g. visitBlockNode:.
	 All such implementations under ParseNode should answer the result of the dispatch, e.g.
		^aVisitor visitBlockNode: self"
	^self subclassResponsibility