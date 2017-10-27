menu messages
internalCallToImplementorsOfIt
	"Open an implementors browser on the selected selector"

	| aSelector |
	self lineSelectAndEmptyCheck: [ ^ self ].
	(aSelector := self selectedSelector) isNil
		ifTrue: [ ^ textArea flash ].
	self internalCallToImplementorsOf: aSelector