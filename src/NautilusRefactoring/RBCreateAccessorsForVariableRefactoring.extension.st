Extension { #name : #RBCreateAccessorsForVariableRefactoring }

{ #category : #'*NautilusRefactoring' }
RBCreateAccessorsForVariableRefactoring >> whatToDisplayIn: aBrowser [ 

	^ (self changes changes 
		select: [:change | {getterMethod. setterMethod} includes: change selector ]) 
		flatCollect: [:change | change whatToDisplayIn: aBrowser ]
]
