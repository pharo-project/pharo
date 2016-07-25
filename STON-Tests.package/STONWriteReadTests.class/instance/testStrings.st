tests
testStrings
	| strings |
	strings := Collection allSubclasses collect: [ :each | each name asString ].
	self serializeAndMaterialize: strings