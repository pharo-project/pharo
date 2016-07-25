testing
definesPoolDictionary: aSymbol 
	(self directlyDefinesPoolDictionary: aSymbol) ifTrue: [^true].
	^self inheritsPoolDictionaries and: 
			[self superclass notNil 
				and: [self superclass definesPoolDictionary: aSymbol]]