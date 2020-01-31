Extension { #name : #RBBlockNode }

{ #category : #'*EnlumineurFormatter' }
RBBlockNode >> allStatements [
	"including temp variable definition."
	^ self temporaries asOrderedCollection copy
			addAll: super allStatements;
			yourself

]

{ #category : #'*EnlumineurFormatter' }
RBBlockNode >> hasArguments [

	^ arguments isNotEmpty
]

{ #category : #'*EnlumineurFormatter' }
RBBlockNode >> hasTemporaries [

	^ self temporaries isNotEmpty
]

{ #category : #'*EnlumineurFormatter' }
RBBlockNode >> headIsNotEmpty [
	"Return true whether the receiver has some arguments or comments"
	
	^ arguments isNotEmpty or: [ self comments isNotEmpty ]
]
