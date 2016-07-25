rendering support
isBreakableAt: index in: text

	| char |
	char := text at: index.
	char = Character space ifTrue: [^ true].
	char = Character cr ifTrue: [^ true].
	^ false.
