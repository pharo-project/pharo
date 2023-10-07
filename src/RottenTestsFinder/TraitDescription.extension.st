Extension { #name : 'TraitDescription' }

{ #category : '*RottenTestsFinder' }
TraitDescription >> generateRottenTestFromCompiledMethod: aCompiledMethod testCase: aTestResult [
	^ RottenTestImplementedInTrait compiledMethod: aCompiledMethod
]
