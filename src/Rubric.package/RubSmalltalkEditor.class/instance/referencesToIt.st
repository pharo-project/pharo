menu messages
referencesToIt
	"Open a references browser on the selected symbol"

	| aSymbol |
	self selectLine.
	(aSymbol := self selectedSymbol) isNil
		ifTrue: [ ^ textArea flash ].
	self referencesTo: aSymbol