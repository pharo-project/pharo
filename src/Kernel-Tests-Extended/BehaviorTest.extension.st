Extension { #name : #BehaviorTest }

{ #category : #'*Kernel-Tests-Extended' }
BehaviorTest >> testAllReferencesTo [
	| result |
	result := SystemNavigation new allReferencesTo: Point binding.
	result do: [ :each | self assert: (each compiledMethod hasLiteral: Point binding) ].
	self assert: (result anySatisfy: [ :each | each methodClass = self class and: [ each selector = #testAllReferencesTo ] ]).
		
	result := SystemNavigation new allReferencesTo: #printOn:.
	result do: [ :each | self assert: (each compiledMethod refersToLiteral: #printOn:) ].
	self assert: (result anySatisfy: [ :each | each methodClass = self class and: [ each selector = #testAllReferencesTo ] ]).
	
	result := SystemNavigation new allReferencesTo: #+.
	result do: [ :each | self assert: ((each compiledMethod sendsSelector: #+) or: [ each compiledMethod refersToLiteral: #+ ]) ].
	self assert: (result anySatisfy: [ :each | each methodClass = self class and: [ each selector = #testAllReferencesTo ] ])
]
