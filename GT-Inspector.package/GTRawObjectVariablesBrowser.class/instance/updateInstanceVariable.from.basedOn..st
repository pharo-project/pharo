private building
updateInstanceVariable: aSymbol from: anObject basedOn: aString
	|result|

	(anObject class hasInstVarNamed: aSymbol) 
		ifFalse: [ ^ self ].
	result := anObject class compiler
			source: aString;
			context: nil;
			receiver: anObject;
			failBlock: [ ^ self ];
			evaluate.
						
	anObject writeSlotNamed: aSymbol value: result.
	