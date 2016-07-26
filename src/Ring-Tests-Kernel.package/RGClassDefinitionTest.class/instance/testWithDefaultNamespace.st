testing
testWithDefaultNamespace
	| newClass |
	
	newClass:= RGClassDefinition named:  #Object.

	self assert: (newClass parent = Smalltalk globals).
	self assert: (newClass realClass = Object).