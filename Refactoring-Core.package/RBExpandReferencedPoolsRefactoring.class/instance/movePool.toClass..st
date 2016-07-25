transforming
movePool: aSymbol toClass: aClass 
	| nonMetaClass |
	nonMetaClass := aClass theNonMetaClass.
	(nonMetaClass definesPoolDictionary: aSymbol) ifFalse: [ nonMetaClass addPoolDictionary: aSymbol ]