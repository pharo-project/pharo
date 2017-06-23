building
composeNotValidIn: composite
	composite actionList
		title: [ :repo | 'Repository {1} is invalid.' format: { repo descriptionText } ];
		beVertical;
		act: [ :presentation :model | 
			model editRepositoryThen: [ 
				"scale to top browser"
				presentation pane browser pane browser update ] ] entitled: 'Edit...';
		act: [ :presentation :model | 
			model forgetThen: [ 
				presentation pane browser pane browser update ] ] entitled: 'Forget'