Extension { #name : #GlobalVariable }

{ #category : #'*Ring2-Core' }
GlobalVariable >> asRing2MinimalDefinitionIn: aRG2Environment [ 

	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		RG2GlobalVariableDefinition named: self key asSymbol parent: aRG2Environment].
]
