example
columnPrototype
	"Answer a prototypical column"
	"self columnPrototype openInHand"
	<exampleWidget>
	
	| sampleMorphs aColumn |
	sampleMorphs := #(red yellow green) collect:
		[:aColor | Morph new extent: 130 @ 38; color: (Color perform: aColor); setNameTo: aColor asString; yourself].
	aColumn := self inAColumn: sampleMorphs.
	aColumn setNameTo: 'Column'.
	aColumn color: Color veryVeryLightGray.
	aColumn cellInset: 4; layoutInset: 6.
	aColumn enableDragNDrop.
	aColumn setBalloonText: 'Things dropped into here will automatically be organized into a column. Once you have added your own items here, you will want to remove the sample colored rectangles that this started with, and you will want to change this balloon help message to one of your own!'.
	^ aColumn