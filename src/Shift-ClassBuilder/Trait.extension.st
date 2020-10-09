Extension { #name : #Trait }

{ #category : #'*Shift-ClassBuilder' }
Trait class >> << aSymbol [
	^ FluidTraitBuilder new 
		name: aSymbol;
		yourself
]
