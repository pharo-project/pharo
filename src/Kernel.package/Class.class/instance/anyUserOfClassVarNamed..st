class variables
anyUserOfClassVarNamed: aSymbol
	self withAllSubclasses do: [ :subclass | 
 		(Array with: subclass with: subclass class) do: [ :classOrMeta | 
			(classOrMeta whichSelectorsReferTo: (self classPool associationAt: aSymbol)) 
				ifNotEmpty: [ ^classOrMeta ]]].
				
	^nil