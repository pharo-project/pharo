handler selection
isResponsibleFor: aCommandLine
	"This handler is reponsible only for .st files"
	(aCommandLine hasFileWithExtension: '.st')
		ifTrue: [ ^ true ].
	^ super isResponsibleFor: aCommandLine