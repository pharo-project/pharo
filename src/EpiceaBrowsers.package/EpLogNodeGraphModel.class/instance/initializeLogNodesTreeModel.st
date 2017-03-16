initialization
initializeLogNodesTreeModel

	self initializeHiedraController.

	logNodesTreeModel
		rowInset: 0;
		columns: {
			TreeColumnModel new	
				displayBlock: [ :item | hiedraRulerController rowMorphFor: item content ];
				initialWidth: 50
				yourself.

			TreeColumnModel new
				displayBlock: [ :item | item content asMorph ];
				yourself.
			}