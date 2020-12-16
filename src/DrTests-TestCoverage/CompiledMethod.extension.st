Extension { #name : #CompiledMethod }

{ #category : #'*DrTests-TestCoverage' }
CompiledMethod >> drTestsName [
	^ self methodClass asString, '>>#', self selector asString
]
