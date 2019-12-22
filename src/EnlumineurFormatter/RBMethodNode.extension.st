Extension { #name : #RBMethodNode }

{ #category : #'*EnlumineurFormatter' }
RBMethodNode >> allStatements [
	"return the statements including variable definition."
	^ self temporaries asOrderedCollection copy
			addAll: super allStatements;
			yourself

]

{ #category : #'*EnlumineurFormatter' }
RBMethodNode >> hasTemporaries [
	^ self temporaries isNotEmpty
]
