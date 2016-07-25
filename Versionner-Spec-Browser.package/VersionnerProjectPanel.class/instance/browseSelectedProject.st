actions
browseSelectedProject
	^ self projectBoundToSelection
		ifNotNil: [ :dependantProject | 
			self browseProject: dependantProject configurationClass ]