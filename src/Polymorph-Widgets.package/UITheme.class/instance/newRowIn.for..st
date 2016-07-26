morph creation
newRowIn: aThemedMorph for: controls
	"Answer a morph laid out with a row of controls."

	|answer|
	answer := PanelMorph new
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		fillStyle: Color transparent; "non pane color tracking"
		changeTableLayout;
		listDirection: #leftToRight;
		cellInset: 8;
		cellPositioning: #center.
	controls do: [:m | answer addMorphBack: m].
	^answer
			