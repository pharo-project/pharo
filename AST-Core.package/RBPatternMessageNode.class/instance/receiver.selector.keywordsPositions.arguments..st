initialize-release
receiver: aValueNode selector: aSymbol keywordsPositions: positionList arguments: valueNodes
	| message |
	self
		receiver: aValueNode;
		selector: aSymbol;
		keywordsPositions: positionList;
		arguments: valueNodes.
		
	isCascadeList := isList := false.
	message := self selectorParts first.
	
	2 to: message size
		do: 
			[:i | 
			| character |
			character := message at: i.
			character == self listCharacter 
				ifTrue: [isList := true]
				ifFalse: 
					[character == self cascadeListCharacter 
						ifTrue: [isCascadeList := true]
						ifFalse: [^self]]]