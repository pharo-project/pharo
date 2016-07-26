building
codeBlockFromStatements: statements
	^ self codeBlock: ((self codeSequence: #()) statements: statements; yourself)