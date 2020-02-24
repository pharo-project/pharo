Extension { #name : #RBRefactoring }

{ #category : #'*NautilusRefactoring' }
RBRefactoring >> whatToDisplayIn: aBrowser [ 
	
	^ (self changes changes collect: [:change | change onSystemDictionary: self model environment]) flatCollect: [:e | e whatToDisplayIn: aBrowser ]
]
