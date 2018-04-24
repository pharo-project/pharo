Extension { #name : #AssociationTest }

{ #category : #'*Kernel-Tests-WithCompiler' }
AssociationTest >> testStoreOnNegativeIntegerRoundtrip [
	| association storeString evaluated |
	association := 'a'-> -1.
	
	storeString := String streamContents: [ :s | association storeOn: s ].
	
	evaluated := Smalltalk compiler evaluate: storeString.
	
	self assert: association equals: evaluated
]

{ #category : #'*Kernel-Tests-WithCompiler' }
AssociationTest >> testStoreOnPositiveIntegerRoundtrip [
	| association storeString evaluated |
	association := 'a'-> 1.
	storeString := String streamContents: [ :s | association storeOn: s ].
	
	evaluated := Smalltalk compiler evaluate: storeString.
	
	self assert: association equals: evaluated
]
