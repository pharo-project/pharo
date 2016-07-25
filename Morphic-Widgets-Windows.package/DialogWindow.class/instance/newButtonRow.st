actions
newButtonRow
	"Answer a new ok/cancel button row."

	|answer buttons e|
	buttons := self newButtons.
	e := 0@0.
	buttons do: [:b | e := e max: b minExtent].
	buttons do: [:b | b extent: e].
	answer := Morph new
		color: Color transparent;
		changeTableLayout;
		cellInset: 8;
		listDirection: #leftToRight;
		listCentering: #bottomRight;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap.
	buttons do: [:b | answer addMorphBack: b].
	^answer