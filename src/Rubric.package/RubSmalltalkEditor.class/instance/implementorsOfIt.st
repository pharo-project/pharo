menu messages
implementorsOfIt
	"Open an implementors browser on the selected selector"

	| aSelector |
	"self lineSelectAndEmptyCheck: [^ self]."
	(aSelector := self selectedSelector) == nil ifTrue: [^ textArea flash].
	self implementorsOf: aSelector