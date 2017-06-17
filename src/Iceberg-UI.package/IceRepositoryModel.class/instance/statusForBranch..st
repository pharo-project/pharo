accessing
statusForBranch: aBranch
	| incoming outgoing allStatus |
	
	incoming := aBranch incomingCommits size.
	outgoing := aBranch outgoingCommits size.
	
	allStatus := OrderedCollection new.
	(incoming = 0 and: [ outgoing = 0 ]) ifTrue: [ allStatus add: 'Up to date' ].
	incoming > 0 ifTrue: [ allStatus add: ('{1} incoming' format: {incoming}) ].
	outgoing > 0 ifTrue: [ allStatus add: ('{1} not published' format: {outgoing}) ].
	^ allStatus asCommaString