menu messages
implementorsOfIt
	"Open an implementors browser on the selected selector"

	| aSelector |
	(aSelector := self selectedSelector) isNil
		ifTrue: [ ^ textArea flash ].
	self implementorsOf: aSelector