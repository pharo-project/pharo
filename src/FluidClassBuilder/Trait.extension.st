Extension { #name : #Trait }

{ #category : #'*FluidClassBuilder' }
Trait class >> << aSymbol [
	"Return a builder of class FluidTraitBuilder. To get a trait, this builder should be sent build. This way we can manipulate and test trait definition without be forced to get them added to the environment."
	
	^ FluidTraitBuilder new
		  name: aSymbol;
		  yourself
]
