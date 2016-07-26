initialization
initializeOn: aGofer
	super initializeOn: aGofer.
	self gofer references do: [ :reference |
		cacheReferences do: [ :resolved |
			(reference matches: resolved) ifTrue: [
				self gofer repositories do: [ :repository |
					((self gofer allResolvedIn: repository) includes: resolved)
						ifFalse: [ self model add: resolved -> repository ] ] ] ] ]