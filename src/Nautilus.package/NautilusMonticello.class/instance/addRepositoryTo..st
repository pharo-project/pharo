monticello menus behavior
addRepositoryTo: aGroup
	| repo |
	(repo := MCTool newRepository) ifNil: [ ^ self ].
	aGroup addRepository: repo.