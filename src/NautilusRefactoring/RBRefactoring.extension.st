Extension { #name : 'RBRefactoring' }

{ #category : '*NautilusRefactoring' }
RBRefactoring >> whatToDisplayIn: aBrowser [

	^ (self changes changes collect: [:change | change onSystemEnvironment: self model environment systemDictionary]) flatCollect: [:e | e whatToDisplayIn: aBrowser ]
]
