stdio
stdout

	^Stdout ifNil: [ Stdout := self standardIOStreamNamed: #stdout forWrite: true ]