private - menu actions
browseMenuAction

	^ { 'Browse'. 
		#browseSelection.
		'Browse working copy of entity affected in the selected event(s)'.
		#nautilusIcon. 
		$b command }