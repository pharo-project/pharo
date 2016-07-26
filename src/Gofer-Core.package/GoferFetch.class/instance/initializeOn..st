initialization
initializeOn: aGofer
	super initializeOn: aGofer.
	self gofer references do: [ :reference |
		self gofer allResolved do: [ :resolved |
			((reference matches: resolved) and: [ (cacheReferences includes: resolved) not ])
				ifTrue: [ self model add: resolved ] ] ]