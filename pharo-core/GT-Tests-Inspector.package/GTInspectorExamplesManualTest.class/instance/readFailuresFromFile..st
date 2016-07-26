private
readFailuresFromFile: aFile
	aFile exists ifFalse: [ ^ OrderedCollection new ].
	^ (((aFile readStream contents splitOn: String cr)
		collect: [ :each | [ Compiler evaluate: each trimBoth ] on: Error do: [ nil ] ])
		reject: [ :each | each isNil ]) asSet asOrderedCollection