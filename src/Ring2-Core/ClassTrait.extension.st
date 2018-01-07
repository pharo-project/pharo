Extension { #name : #ClassTrait }

{ #category : #'*Ring2-Core' }
ClassTrait >> asRing2MinimalDefinitionIn: aRG2Environment [

	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		RG2MetaclassTraitDefinition named: self name parent: aRG2Environment]

]
