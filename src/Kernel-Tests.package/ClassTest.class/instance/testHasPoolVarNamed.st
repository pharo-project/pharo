tests - access
testHasPoolVarNamed
	"self debug: #testHasPoolVarNamed"

	self assert: (Date usesLocalPoolVarNamed: 'DayNames').
	"a metaclass does not have shared pools since only classes have shared pools"
	self deny: (Date class usesLocalPoolVarNamed: 'DayNames').
	
	self assert: (RootClassPoolUser usesLocalPoolVarNamed: 'Author').
	"a subclass does not have the one of its superclass - but it would be good to change that"
	self deny: (SubclassPoolUser usesLocalPoolVarNamed: 'Author').