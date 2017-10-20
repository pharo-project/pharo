tests - access
testUsesPoolVarNamed
	"self debug: #testUsesPoolVarNamed"

	self assert: (Date usesPoolVarNamed: 'DayNames').
	"a metaclass does not have shared pools since only classes have shared pools"
	self deny: (Date class usesPoolVarNamed: 'DayNames').
	
	self assert: (RootClassPoolUser usesPoolVarNamed: 'Author').
	"a subclass  has  the one of its superclass"
	self assert: (SubclassPoolUser usesPoolVarNamed: 'Author').