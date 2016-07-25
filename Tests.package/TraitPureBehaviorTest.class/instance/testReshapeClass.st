testing-applying trait composition
testReshapeClass
	"self run: #testReshapeClass"

	"Ensure that reshaping a class has no impact on its traits"

	self assert: self c2 traitCompositionString = 'T5 - {#m11}'.
	self assert: self c2 selectors sorted =  #(bar foo m12 m13 m21 m22 m51 m52 m53).
	self assert: self c2 localSelectors sorted =  #(bar foo).

	self c2 addInstVarNamed: 'foobar'.

	self assert: self c2 traitCompositionString = 'T5 - {#m11}'.
	self assert: self c2 selectors sorted =  #(bar foo m12 m13 m21 m22 m51 m52 m53).
	self assert: self c2 localSelectors sorted =  #(bar foo).

	self c2 removeInstVarNamed: 'foobar'.

	self assert: self c2 traitCompositionString = 'T5 - {#m11}'.
	self assert: self c2 selectors sorted =  #(bar foo m12 m13 m21 m22 m51 m52 m53).
	self assert: self c2 localSelectors sorted =  #(bar foo).