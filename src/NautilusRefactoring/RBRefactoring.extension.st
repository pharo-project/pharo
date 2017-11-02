Extension { #name : #RBRefactoring }

{ #category : #'*NautilusRefactoring' }
RBRefactoring >> whatToDisplayIn: aBrowser [ 
	
	^ self changes changes flatCollect: [:e | e whatToDisplayIn: aBrowser ]
]
