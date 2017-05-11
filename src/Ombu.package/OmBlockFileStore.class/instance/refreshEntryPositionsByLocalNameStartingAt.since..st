private
refreshEntryPositionsByLocalNameStartingAt: firstStreamPosition since: initialLocalName
	"AKA: Refresh blocks after new entries were written."

	| lastBlock |
	lastBlock := blocks last.

	"Update last block if necessary"
	firstStreamPosition < lastBlock endPosition ifTrue: [ 
		lastBlock refreshIfNeededStartingAt: firstStreamPosition since: initialLocalName ].

	"Add new blocks if necessary"
	lastStreamPosition > lastBlock endPosition ifTrue: [
		self readEntriesWith: [:readStream | self refreshNewBlocksFrom: readStream ] ].
