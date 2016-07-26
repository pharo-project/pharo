private
readClassesFromFile: aFile
	aFile exists ifFalse: [ ^ OrderedCollection new ].
	^ ((aFile readStream contents splitOn: String cr) 
		collect: [ :each | [ Smalltalk classNamed: each trimBoth ] on: Error do: [ nil ] ]) 
		reject: [ :each | each isNil ]