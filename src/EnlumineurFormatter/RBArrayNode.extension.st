Extension { #name : #RBArrayNode }

{ #category : #'*EnlumineurFormatter' }
RBArrayNode >> allStatements [
	^ statements asOrderedCollection copy
			addAll: super allStatements;
			yourself

]
