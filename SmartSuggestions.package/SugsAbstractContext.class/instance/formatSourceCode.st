refactoring
formatSourceCode
	| formatted |

	formatted := selectedNode formattedCode.
	formatted = self code ifTrue: [ ^ self ].
	self sourceTextArea
		formatSourceCode;
		hasUnacceptedEdits: true