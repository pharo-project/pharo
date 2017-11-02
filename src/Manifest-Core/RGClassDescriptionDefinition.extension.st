Extension { #name : #RGClassDescriptionDefinition }

{ #category : #'*Manifest-Core' }
RGClassDescriptionDefinition >> arrayStringForManifest [
	| array |
	array := Array new: 2.
	array at: 1 put: self class asString asSymbol.
	array at: 2 put: {(name asSymbol)}.
	^ array
]

{ #category : #'*manifest-core' }
RGClassDescriptionDefinition class >> manifestReadOn: aArray [

	^ self named:( aArray first)
]
