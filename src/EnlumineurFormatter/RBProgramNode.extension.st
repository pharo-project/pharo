Extension { #name : #RBProgramNode }

{ #category : #'*EnlumineurFormatter' }
RBProgramNode >> allStatements [
	| children |
	children := self children.
	children isEmpty ifTrue: [^#()].
	^children inject: OrderedCollection new
		into: 
			[:vars :each | 
			vars
				addAll: each allStatements;
				yourself]
]
