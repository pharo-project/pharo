instance-creation
newMapMorph
	"Answer a new map morph."

	^(DiffMapMorph new
		hResizing: #shrinkWrap;
		vResizing: #spaceFill;
		extent: 20@4;
		minWidth: 20;
		borderStyle: (BorderStyle inset width: 1))
			when: #mapClicked
			send: #mapClicked:
			to: self