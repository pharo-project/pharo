Extension { #name : #CompiledMethod }

{ #category : #'*DrTests-TestCoverage' }
CompiledMethod >> drTestsBrowse [
	Smalltalk tools browser openOnClass: self methodClass selector: self selector
]

{ #category : #'*DrTests-TestCoverage' }
CompiledMethod >> drTestsName [
	^ self selector asString
]
