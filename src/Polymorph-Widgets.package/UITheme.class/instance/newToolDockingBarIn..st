morph creation
newToolDockingBarIn: aThemedMorph
	"Answer a new tool docking bar."

	^ToolDockingBarMorph new
		borderWidth: 0;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		clipSubmorphs: true