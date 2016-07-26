accessing
loadRepositoryUrl
	| possibleUrl |
	possibleUrl := self argumentAt: 1.
	(possibleUrl includesSubstring: '://')
		ifTrue: [ ^ possibleUrl asUrl ].
	^ possibleUrl asFileReference