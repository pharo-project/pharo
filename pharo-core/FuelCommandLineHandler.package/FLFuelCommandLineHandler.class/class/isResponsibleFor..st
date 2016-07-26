handler selection
isResponsibleFor: aCommandLine
	"This handler is reponsible only for .fuel files"
	(aCommandLine hasFileWithExtension: self fileExtension)
		ifTrue: [ ^ true ].
	^ super isResponsibleFor: aCommandLine
	