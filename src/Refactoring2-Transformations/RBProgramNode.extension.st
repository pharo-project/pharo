Extension { #name : #RBProgramNode }

{ #category : #'*Refactoring2-Transformations' }
RBProgramNode >> allAccesses [

	^ self allChildren select: #isVariable
]

{ #category : #'*Refactoring2-Transformations' }
RBProgramNode >> parentSuchAs: aBlock [

	^ (self parent perform: aBlock)
		ifTrue: [ self parent ]
		ifFalse: [ ^ self parent parentSuchAs: aBlock ]
]

{ #category : #'*Refactoring2-Transformations' }
RBProgramNode >> sourceCode [

	| interval |
	interval := self sourceInterval.
	interval ifEmpty: [ ^ '' ].
	
	^ self source 
		copyFrom: interval first
		to: interval last
]
