utils - morph creation
alignmentMorph: aCollectionOfMorphs
	"Answer a row-oriented AlignmentMorph holding the given collection"
	| morph |

	morph:= AlignmentMorph new
		listDirection: #leftToRight;
		borderWidth: 0;
		color: Color transparent;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap;
		layoutInset: 1;
		wrapCentering: #center;
		cellPositioning: #center.
	aCollectionOfMorphs do: [ :each | morph addMorphBack: each].
	^morph