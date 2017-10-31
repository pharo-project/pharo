menu messages
implementorsOfIt
	"Open an implementors browser on the selected selector"

	| aSelector |
	(aSelector := self selectedSelector) ifNil: [^ textArea flash].
	self implementorsOf: aSelector
