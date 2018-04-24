accessing
originRepository
	| repositoryReference |
	repositoryReference := self repositoryLocation / 'src'.

	^ originRepository
		ifNil: [ originRepository := TonelRepository new
				directory: repositoryReference;
				yourself]