Extension { #name : #RBSequenceNode }

{ #category : #'*EnlumineurFormatter' }
RBSequenceNode >> allStatements [
	^ statements asOrderedCollection copy
			addAll: super allStatements;
			yourself

]

{ #category : #'*EnlumineurFormatter' }
RBSequenceNode >> hasTemporaries [

	^ temporaries isNotEmpty
]
