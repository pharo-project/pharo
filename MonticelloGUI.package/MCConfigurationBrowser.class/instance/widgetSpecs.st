morphic ui
widgetSpecs
	^ #(
		((buttonRow) (0 0 1 0) (0 0 0 30))
		((listMorph:selection:menu: dependencyList dependencyIndex dependencyMenu:) (0 0 1 1) (0 30 0 -180))
		((listMorph:selection:menu: repositoryList repositoryIndex repositoryMenu:) (0 1 1 1) (0 -180 0 -120))
		((textMorph: description) (0 1 1 1) (0 -120 0 0))
	 	)