Extension { #name : #BehaviorTest }

{ #category : #'*Kernel-Tests-Extended' }
BehaviorTest >> testAllReferencesTo [
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
]

{ #category : #'*Kernel-Tests-Extended' }
BehaviorTest >> testWhichSelectorsAccess [
	self assert: ((Point whichSelectorsAccess: 'x') includes: #x).
	self deny:  ((Point whichSelectorsAccess: 'y') includes: #x).
]
