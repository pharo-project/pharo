private - menu actions
redoMenuAction

	^ { 'Redo'.
		#redoSelection.
		'Redo selected event(s)'.
		#smallRedoIcon.
		$r command }