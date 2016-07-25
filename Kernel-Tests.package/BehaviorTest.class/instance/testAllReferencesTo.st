tests
testAllReferencesTo
	| result |
	result := SystemNavigation new allReferencesTo: Point binding.
	result do: [ :each | self assert: (each compiledMethod hasLiteral: Point binding) ].
	self assert: (result anySatisfy: [ :each | each actualClass = self class and: [ each selector = #testAllReferencesTo ] ]).
		
	result := SystemNavigation new allReferencesTo: #printOn:.
	result do: [ :each | self assert: (each compiledMethod hasLiteralThorough: #printOn:) ].
	self assert: (result anySatisfy: [ :each | each actualClass = self class and: [ each selector = #testAllReferencesTo ] ]).
	
	result := SystemNavigation new allReferencesTo: #+.
	result do: [ :each | self assert: ((each compiledMethod sendsSelector: #+) or: [ each compiledMethod hasLiteralThorough: #+ ]) ].
	self assert: (result anySatisfy: [ :each | each actualClass = self class and: [ each selector = #testAllReferencesTo ] ])