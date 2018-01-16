Extension { #name : #RG2ClassDefinition }

{ #category : #'*Manifest-Core' }
RG2ClassDefinition >> arrayStringForManifest [
	| array |
	array := Array new: 2.
	array at: 1 put: self class name asSymbol.
	array at: 2 put: {(self name asSymbol)}.
	^ array
]

{ #category : #'*Manifest-Core' }
RG2ClassDefinition class >> manifestReadOn: aArray [

	^ self named:( aArray first)
]
