Extension { #name : #Metaclass }

{ #category : #'*Ring2-Core' }
Metaclass >> asRing2MinimalDefinitionIn: aRG2Environment [

	^ RG2MetaclassDefinition named: self name parent: aRG2Environment.

]
