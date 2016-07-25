instance creation
newTasksMorph
	"Answer a new task list."

	^Morph new
		changeTableLayout;
		listDirection: #leftToRight;
		wrapDirection: #topToBottom;
		cellInset: 1;
		color: Color transparent;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		yourself.