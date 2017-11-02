Extension { #name : #RBSwapMethodRefactoring }

{ #category : #'*NautilusRefactoring' }
RBSwapMethodRefactoring >> whatToDisplayIn: aBrowser [ 

	^ (self changes changes 
		select: [:change | selector = change selector ]) 
		flatCollect: [:change | change whatToDisplayIn: aBrowser ]
]
