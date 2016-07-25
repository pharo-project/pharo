accessing
stdin

	^ Stdin ifNil: [ Stdin := self standardIOStreamNamed: #stdin forWrite: false ].