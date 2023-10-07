Extension { #name : 'RBCreateAccessorsForVariableTransformation' }

{ #category : '*NautilusRefactoring' }
RBCreateAccessorsForVariableTransformation >> whatToDisplayIn: aBrowser [

	^ (self changes changes
		select: [:change | {getterMethodName. setterMethodName} includes: change selector ])
		flatCollect: [:change | change whatToDisplayIn: aBrowser ]
]
