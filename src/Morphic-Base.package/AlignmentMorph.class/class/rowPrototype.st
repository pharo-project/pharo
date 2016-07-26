example
rowPrototype
	"Answer a prototypical row"
	"self rowPrototype openInHand"
	<exampleWidget>
	
	| sampleMorphs aRow |
	sampleMorphs := (1 to: (2 + 3 atRandom)) collect:
		[:integer | EllipseMorph new extent: ((60 + (20 atRandom)) @ (80 + ((20 atRandom)))); color: Color random; setNameTo: ('egg',  integer asString); yourself].
	aRow := self inARow: sampleMorphs.
	aRow setNameTo: 'Row'.
	aRow enableDragNDrop.
	aRow cellInset: 6.
	aRow layoutInset: 8.
	aRow setBalloonText: 'Things dropped into here will automatically be organized into a row. Once you have added your own items here, you will want to remove the sample colored eggs that this started with, and you will want to change this balloon help message to one of your own!'.
	aRow color: Color veryVeryLightGray.
	^ aRow

			