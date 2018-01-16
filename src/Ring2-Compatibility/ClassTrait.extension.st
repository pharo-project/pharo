Extension { #name : #ClassTrait }

{ #category : #'*Ring2-Compatibility' }
ClassTrait >> asRing2Definition [

	^ self theNonMetaClass asRing2Definition theMetaClass
]

{ #category : #'*Ring2-Compatibility' }
ClassTrait >> asRing2DefinitionIn: aRG2Environment [

	^ RG2MetaclassTraitDefinition named: self name parent: aRG2Environment.

]
