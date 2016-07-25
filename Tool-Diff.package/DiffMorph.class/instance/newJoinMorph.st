instance-creation
newJoinMorph
	"Answer a new join morph."

	| w |
	w := DiffJoinMorph splitterWidth.
	^ DiffJoinMorph new
		hResizing: #shrinkWrap;
		vResizing: #spaceFill;
		extent: w @ 4;
		minWidth: w;
		color: self joinColor