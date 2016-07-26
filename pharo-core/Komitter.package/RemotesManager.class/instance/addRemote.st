private
addRemote
	
	MCTool newRepository ifNotNil: [ :repo | 
		MCRepositoryGroup default addRepository: repo.
		remotes roots: self buildRoots ]