private building
updateVariableNode: aNode basedOn: aString
	| aResult |

	aResult := aNode hostObject class compiler
			source: aString;
			context: nil;
			receiver: aNode hostObject;
			failBlock: [ ^ self ];
			evaluate.
						
	aNode save: aResult