Extension { #name : #CompiledMethod }

{ #category : #'*DrTests-TestCoverage' }
CompiledMethod >> drTestsName [
	^ self selector asString
]
