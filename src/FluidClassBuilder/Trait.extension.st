Extension { #name : #Trait }

{ #category : #'*FluidClassBuilder' }
Trait class >> << aSymbolOrAClassSideTrait [

	"Return a builder of class FluidTraitBuilder. To get a trait, this builder should be sent build. This way we can manipulate and test trait definition without be forced to get them added to the environment."

	aSymbolOrAClassSideTrait isSymbol
		ifFalse: [
			^ FluidClassSideTraitBuilder new
				  nameToBuild: aSymbolOrAClassSideTrait soleInstance name;
				  fillInstanceSideFromClass: aSymbolOrAClassSideTrait soleInstance;
				  yourself ].

	^ FluidTraitBuilder new
			nameToBuild: aSymbolOrAClassSideTrait;
			fillClassSideFromEnvironment: self environment;
			yourself
]
