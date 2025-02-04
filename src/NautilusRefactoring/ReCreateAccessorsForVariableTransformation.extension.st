Extension { #name : 'ReCreateAccessorsForVariableTransformation' }

{ #category : '*NautilusRefactoring' }
ReCreateAccessorsForVariableTransformation >> whatToDisplayIn: aBrowser [

	^ (self changes changes
		select: [:change | {getterMethodName. setterMethodName} includes: change selector ])
		flatCollect: [:change | change whatToDisplayIn: aBrowser ]
]
