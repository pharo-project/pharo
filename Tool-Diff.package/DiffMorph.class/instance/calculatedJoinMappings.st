actions
calculatedJoinMappings
	"Calculate the join parameters between src and dst and answer"

	| sourceLine destinationLine joins destinationRunStart sourceRunStart destinationRunEnd sourceRunEnd matchDestinationStart matchSourceStart |
	sourceLine := destinationLine := 0.
	joins := OrderedCollection new.
	destinationRunStart := destinationRunEnd := sourceRunStart := sourceRunEnd := matchSourceStart := matchDestinationStart := 0.
	self difference
		do: [ :p | 
			p key = #match
				ifTrue: [ 
					sourceLine := sourceLine + 1.
					destinationLine := destinationLine + 1.
					matchSourceStart = 0
						ifTrue: [ 
							matchSourceStart := sourceLine.
							matchDestinationStart := destinationLine ].
					(destinationRunStart > 0 or: [ sourceRunStart > 0 ])
						ifTrue: [ 
							sourceRunStart = 0
								ifTrue: [ sourceRunStart := sourceLine ].
							destinationRunStart = 0
								ifTrue: [ destinationRunStart := destinationLine ].
							sourceRunEnd = 0
								ifTrue: [ sourceRunEnd := sourceRunStart - 1 ].
							destinationRunEnd = 0
								ifTrue: [ destinationRunEnd := destinationRunStart - 1 ].
							joins
								add:
									(self newJoinSectionFrom: (sourceRunStart to: sourceRunEnd) to: (destinationRunStart to: destinationRunEnd)).
							destinationRunStart := destinationRunEnd := sourceRunStart := sourceRunEnd := 0 ] ].
			p key = #remove
				ifTrue: [ 
					matchSourceStart > 0
						ifTrue: [ 
							joins
								add:
									(self newMatchJoinSectionFrom: (matchSourceStart to: sourceLine) to: (matchDestinationStart to: destinationLine)).
							matchSourceStart := matchDestinationStart := 0 ].
					sourceLine := sourceLine + 1.
					sourceRunStart = 0
						ifTrue: [ sourceRunStart := sourceLine ].
					sourceRunEnd := sourceLine ].
			p key = #insert
				ifTrue: [ 
					matchSourceStart > 0
						ifTrue: [ 
							joins
								add:
									(self newMatchJoinSectionFrom: (matchSourceStart to: sourceLine) to: (matchDestinationStart to: destinationLine)).
							matchSourceStart := matchDestinationStart := 0 ].
					destinationLine := destinationLine + 1.
					sourceRunStart > 0
						ifTrue: [ 
							sourceRunEnd = 0
								ifTrue: [ sourceRunEnd := sourceRunStart ].
							destinationRunEnd = 0
								ifTrue: [ destinationRunEnd := destinationRunStart ].
							joins
								add:
									(self newJoinSectionFrom: (sourceRunStart to: sourceRunEnd) to: (destinationRunStart to: destinationRunEnd)).
							destinationRunStart := destinationRunEnd := sourceRunStart := sourceRunEnd := 0 ].
					destinationRunStart = 0
						ifTrue: [ destinationRunStart := destinationLine ].
					destinationRunEnd := destinationLine ] ].
	sourceLine := sourceLine + 1.
	destinationLine := destinationLine + 1.
	(destinationRunStart > 0 or: [ sourceRunStart > 0 ])
		ifTrue: [ 
			sourceRunStart = 0
				ifTrue: [ sourceRunStart := sourceLine ].
			destinationRunStart = 0
				ifTrue: [ destinationRunStart := destinationLine ].
			sourceRunEnd = 0
				ifTrue: [ sourceRunEnd := sourceRunStart - 1 ].
			destinationRunEnd = 0
				ifTrue: [ destinationRunEnd := destinationRunStart - 1 ].
			joins
				add: (self newJoinSectionFrom: (sourceRunStart to: sourceRunEnd) to: (destinationRunStart to: destinationRunEnd)) ].
	matchSourceStart > 0
		ifTrue: [ 
			joins
				add:
					(self newMatchJoinSectionFrom: (matchSourceStart to: sourceLine - 1) to: (matchDestinationStart to: destinationLine - 1)) ].
	^ joins