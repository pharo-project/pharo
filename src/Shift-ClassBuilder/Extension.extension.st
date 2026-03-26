Extension { #name : 'Extension' }

{ #category : '*Shift-ClassBuilder' }
Extension class >> << aSymbolOrAClassSideTrait [
	"Return a builder of class FluidTraitBuilder. To get a trait, this builder should be sent build. This way we can manipulate and test trait definition without be forced to get them added to the environment."

	aSymbolOrAClassSideTrait isSymbol ifFalse: [
		^ ShiftClassSideBuilder new
			  beExtension;
			  name: aSymbolOrAClassSideTrait soleInstance name;
			  fillInstanceSideFromClass: aSymbolOrAClassSideTrait soleInstance;
			  yourself ].

	^ ShiftClassBuilder new
		  beExtension;
		  name: aSymbolOrAClassSideTrait;
		  fillClassSideFromEnvironment: self environment;
		  yourself
]
