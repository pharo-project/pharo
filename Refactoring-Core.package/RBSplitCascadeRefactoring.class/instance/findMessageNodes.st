preconditions
findMessageNodes
	"Find the nodes that form the first part of the cascade and the second part of the cascade."

	beforeNodes := cascadeNode messages
		select: [ :each | each stop <= selectedInterval first ].
	afterNodes := cascadeNode messages
		select: [ :each | selectedInterval last <= each keywordsPositions first].
	(beforeNodes isEmpty or: [ afterNodes isEmpty ])
		ifTrue: [ self refactoringFailure: 'Splitting a cascade into the whole cascade and an empty one is pointless' ].
	(beforeNodes size + afterNodes size = cascadeNode messages size)
		ifFalse: [ self refactoringFailure: 'To set the split boundary place the cursor inbetween two cascaded messages' ]