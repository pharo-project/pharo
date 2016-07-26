private
removeRemote
	| item |
	
	item := remotes highlightedItem content.
	MCRepositoryGroup default
		removeRepository: item remote.
		
	remotes 
		roots: self buildRoots;
		highlightedItem: nil