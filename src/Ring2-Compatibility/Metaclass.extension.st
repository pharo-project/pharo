Extension { #name : #Metaclass }

{ #category : #'*Ring2-Compatibility' }
Metaclass >> asRing2Definition [

	^ self theNonMetaClass asRing2Definition theMetaClass
]

{ #category : #'*Ring2-Compatibility' }
Metaclass >> asRing2DefinitionIn: aRG2Environment [

	^ RG2MetaclassDefinition named: self name parent: aRG2Environment.

]
