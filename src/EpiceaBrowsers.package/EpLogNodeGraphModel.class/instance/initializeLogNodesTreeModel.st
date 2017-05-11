initialization
initializeLogNodesTreeModel

	self initializeHiedraController.

	logNodesTreeModel columns:	{	
		hiedraRulerController newTreeColumnModel
			initialWidth: 50
			yourself.

		TreeColumnModel new
			displayBlock: [ :item | item content asMorph ];
			yourself.
	}.
