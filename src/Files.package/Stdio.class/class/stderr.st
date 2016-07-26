accessing
stderr

	^ Stderr ifNil: [ Stderr := self standardIOStreamNamed: #stderr forWrite: true ].