menu messages
internalCallToImplementorsOfIt
	"Open an implementors browser on the selected selector"

	| aSelector |
	self lineSelectAndEmptyCheck: [^ self].
	(aSelector := self selectedSelector) == nil ifTrue: [^ morph flash].
	self internalCallToImplementorsOf: aSelector.
			
	