rendering
buttonRowFrom: aCollectionOfButtons
	"Answer a new ok/cancel button row."

	|answer curentButtons e|
	curentButtons := aCollectionOfButtons.
	e := 0@0.
	curentButtons do: [:b | e := e max: b minExtent].
	curentButtons do: [:b | b extent: e].
	answer := Morph new
		color: Color transparent;
		changeTableLayout;
		cellInset: 8;
		listDirection: #leftToRight;
		listCentering: #bottomRight;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap.
	curentButtons do: [:b | answer addMorphBack: b].
	^answer

	