Extension { #name : #Trait }

{ #category : #'*FluidClassBuilder' }
Trait class >> << aSymbol [
	^ FluidTraitBuilder new 
		name: aSymbol;
		yourself
]
