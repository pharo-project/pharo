execute
execute

	MCTool newRepository ifNotNil:
		[:repos | MCRepositoryGroup default addRepository: repos ].


	