tests
testRenamePrimitive
	| refactoring count |
	count := 0.
	model allReferencesTo: #basicAt:put: do: [:method | count := count + 1].
	refactoring := RBRenameMethodRefactoring 
				model: model
				renameMethod: #basicAt:put:
				in: Object
				to: ('at:' , 'bar:') asSymbol
				permutation: (1 to: 2).
	self proceedThroughWarning: [self executeRefactoring: refactoring].
	model allReferencesTo: #basicAt:put:
		do: 
			[:method | 
			count := count - 1.
			self assert: method source isNil].
	model allReferencesTo: ('at:' , 'bar:') asSymbol
		do: [:method | count := count - 1].
	self assert: count = 0