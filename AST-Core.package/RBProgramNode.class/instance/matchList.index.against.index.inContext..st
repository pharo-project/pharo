matching
matchList: matchNodes index: matchIndex against: programNodes index: programIndex inContext: aDictionary 
	| node currentIndex currentDictionary nodes |
	matchNodes size < matchIndex ifTrue: [^programNodes size < programIndex].
	node := matchNodes at: matchIndex.
	node isList 
		ifTrue: 
			[currentIndex := programIndex - 1.
			
			[currentDictionary := aDictionary copy.
			programNodes size < currentIndex or: 
					[nodes := programNodes copyFrom: programIndex to: currentIndex.
					(currentDictionary at: node ifAbsentPut: [nodes]) = nodes and: 
							[(self 
								matchList: matchNodes
								index: matchIndex + 1
								against: programNodes
								index: currentIndex + 1
								inContext: currentDictionary) 
									ifTrue: 
										[currentDictionary 
											keysAndValuesDo: [:key :value | aDictionary at: key put: value].
										^true].
							false]]] 
					whileFalse: [currentIndex := currentIndex + 1].
			^false].
	programNodes size < programIndex ifTrue: [^false].
	(node match: (programNodes at: programIndex) inContext: aDictionary) 
		ifFalse: [^false].
	^self 
		matchList: matchNodes
		index: matchIndex + 1
		against: programNodes
		index: programIndex + 1
		inContext: aDictionary