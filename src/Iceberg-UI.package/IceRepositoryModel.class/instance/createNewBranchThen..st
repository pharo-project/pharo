actions
createNewBranchThen: aBlock
	| selection |
	selection := UIManager default
		request: 'Please provide the name of the new branch to be created'
		initialAnswer: ''
		title: 'Create new branch'.

	selection ifNotNil: [ 
		selection 
			ifEmpty: [ 
				UIManager default 
					abort: 'Branch name can not be empty.' 
					title: 'Create new branch'. ]
			ifNotEmpty: [ 
				self repository createBranch: selection.
				aBlock cull: self repository branch ] ]