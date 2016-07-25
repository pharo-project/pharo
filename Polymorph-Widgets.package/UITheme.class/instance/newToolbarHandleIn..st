morph creation
newToolbarHandleIn: aThemedMorph
	"Answer a new toolbar handle."

	^PanelMorph new
		fillStyle: Color transparent; "non pane color tracking"
		borderStyle: (BorderStyle raised baseColor: Color blue; width: 1);
		extent: 4@3;
		vResizing: #spaceFill