Extension { #name : 'Trait' }

{ #category : '*Shift-ClassBuilder' }
Trait class >> << aSymbolOrAClassSideTrait [
	"Return a builder of class FluidTraitBuilder. To get a trait, this builder should be sent build. This way we can manipulate and test trait definition without be forced to get them added to the environment."

	aSymbolOrAClassSideTrait isSymbol ifFalse: [
		^ ShiftClassSideBuilder new
			  beTrait;
			  name: aSymbolOrAClassSideTrait soleInstance name;
			  fillInstanceSideFromClass: aSymbolOrAClassSideTrait soleInstance;
			  yourself ].

	^ ShiftClassBuilder new
		  beTrait;
		  name: aSymbolOrAClassSideTrait;
		  fillClassSideFromEnvironment: self environment;
		  yourself
]
