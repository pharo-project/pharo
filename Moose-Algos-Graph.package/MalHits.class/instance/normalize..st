running
normalize: aCollectionOfNodes

	| authNorm hubNorm |
	authNorm := aCollectionOfNodes inject: 0 into: [:s :n| s + n auth].
	hubNorm := aCollectionOfNodes inject: 0 into: [:s :n| s + n hub].
	authNorm = 0 ifTrue: [authNorm := 1].
	hubNorm = 0 ifTrue: [hubNorm := 1].
	aCollectionOfNodes do: [:n|
		n auth: (n auth / authNorm).
		n hub: (n hub / hubNorm)]