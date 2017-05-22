accessing
statusForRemote: aRemote
	| incoming outgoing allStatus |
	
	incoming := (self repository incomingCommitsFrom: aRemote) size.
	outgoing := (self repository outgoingCommitsTo: aRemote) size.
	
	allStatus := OrderedCollection new.
	(incoming = 0 and: [ outgoing = 0 ]) ifTrue: [ allStatus add: 'Up to date' ].
	incoming > 0 ifTrue: [ allStatus add: ('{1} incoming' format: {incoming}) ].
	outgoing > 0 ifTrue: [ allStatus add: ('{1} not published' format: {outgoing}) ].
	^ ((self tagsForRemote: aRemote), allStatus) asCommaString