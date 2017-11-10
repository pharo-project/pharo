Extension { #name : #RGPackage }

{ #category : #'*Manifest-Core' }
RGPackage >> arrayStringForManifest [
	| array |
	array := Array new: 2.
	array at: 1 put: self class asString asSymbol.
	array at: 2 put: {(name asSymbol)}.
	^ array
]

{ #category : #'*Manifest-Core' }
RGPackage class >> manifestReadOn: aArray [

	^ self named:( aArray first)
]
