Extension { #name : 'ReRefactoring' }

{ #category : '*NautilusRefactoring' }
ReRefactoring >> whatToDisplayIn: aBrowser [

	^ (self changes changes collect: [:change | change onSystemEnvironment: self model environment systemDictionary]) flatCollect: [:e | e whatToDisplayIn: aBrowser ]
]
