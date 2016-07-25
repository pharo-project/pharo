menu messages
referencesToIt
	"Open a references browser on the selected symbol"

	| aSymbol env ref |
	self selectLine.
	((aSymbol := self selectedSymbol) == nil)
			ifTrue: [^ textArea flash].

	self referencesTo: aSymbol