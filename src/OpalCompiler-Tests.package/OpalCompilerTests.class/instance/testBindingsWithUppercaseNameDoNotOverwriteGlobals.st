tests
testBindingsWithUppercaseNameDoNotOverwriteGlobals
	| result |
	result := Smalltalk compiler
		bindings: { #UndefinedObject -> Object } asDictionary;
		evaluate: 'UndefinedObject class'.
		
	self assert: result = UndefinedObject class.