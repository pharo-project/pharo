scanning
messages
	"Answer a Set of all the message selectors sent by this method."
	| aSet |
	aSet := self localMessages.
	self innerCompiledBlocksDo: [ :cb | aSet := aSet , cb messages ].
	^ aSet 