building
composeBrowserOn: browser
	browser 
		row: #repositories; 
		row: #info.

	browser transmit 
		to: #repositories; 
		andShow: [ :presentation :repositories | 
			self composeRepositories: repositories in: presentation ].
	browser transmit 
		from: #repositories; 
		to: #info; 
		andShow: [ :a |
			(a custom: GLMWrapper new)
				show: [ :wrapper | self composeRepositoryInfoIn: wrapper ];
				when: [ :model | model isValid ].
			(a custom: GLMWrapper new) 
				show: [ :wrapper | self composeNotValidIn: wrapper ];
				when: [ :model | model isValid not ] ].
		