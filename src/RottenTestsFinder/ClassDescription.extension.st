Extension { #name : 'ClassDescription' }

{ #category : '*RottenTestsFinder' }
ClassDescription >> generateRottenTestFromCompiledMethod: aCompiledMethod testCase: aTestCase [
	^ aCompiledMethod methodClass = aTestCase class
		ifTrue: [ RottenTest compiledMethod: aCompiledMethod ]
		ifFalse: [ "Test is implemented in super class"
			RottenTestImplementedInSuperClass compiledMethod: aCompiledMethod subclassInWhichTestIsRotten: aTestCase class ]
]
