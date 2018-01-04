Extension { #name : #RG2PackageDefinition }

{ #category : #'*Manifest-Core' }
RG2PackageDefinition >> arrayStringForManifest [
	| array |
	array := Array new: 2.
	array at: 1 put: self class name asSymbol.
	array at: 2 put: {(name asSymbol)}.
	^ array
]

{ #category : #'*Manifest-Core' }
RG2PackageDefinition class >> manifestReadOn: aArray [

	^ self named: (aArray first)
]
