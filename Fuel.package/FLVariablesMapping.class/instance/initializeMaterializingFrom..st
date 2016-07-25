serialize/materialize
initializeMaterializingFrom: aDecoder

	notIgnoredVariables := (1 to: aDecoder nextEncodedByte) collect: [ :index | aDecoder nextEncodedString ].

	mapping := notIgnoredVariables  
		collect: [ :name | theClass instVarIndexFor: name ifAbsent: [ nil ] ].

