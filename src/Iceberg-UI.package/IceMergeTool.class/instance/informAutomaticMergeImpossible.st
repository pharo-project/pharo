utilities
informAutomaticMergeImpossible
	self uiManager
		abort: 'Automatic merge is not possible, 
to merge manually please first checkout 
branch ', self targetBranch name, ' and then retry.' 
		title: 'Automatic merge impossible'