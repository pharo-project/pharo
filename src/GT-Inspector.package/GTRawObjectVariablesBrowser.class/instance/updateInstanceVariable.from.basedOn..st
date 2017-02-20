private building
updateInstanceVariable: aSymbol from: anObject basedOn: aString
	| aResult |

	self
		assert: [ anObject class hasInstVarNamed: aSymbol ]
		description: [ 'Instance variable ', aSymbol asString, ' must exist in ', anObject asString ].

	aResult := anObject class compiler
			source: aString;
			context: nil;
			receiver: anObject;
			failBlock: [ ^ self ];
			evaluate.
						
	anObject writeSlotNamed: aSymbol value: aResult