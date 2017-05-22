class initialization
buildDeclaration
	| dictionary |
	self removeOldAccessors.
	dictionary := self literalDeclaration.
	self compileAccessorsFrom: dictionary.
	
	^ declaration := dictionary