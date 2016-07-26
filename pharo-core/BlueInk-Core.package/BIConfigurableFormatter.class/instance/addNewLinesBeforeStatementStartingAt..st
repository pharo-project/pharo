private
addNewLinesBeforeStatementStartingAt: anInteger 
	| newLines |
	newLines := self minimumNewLinesBetweenStatements 
				max: (self retainBlankLinesBetweenStatements 
						ifTrue: [self newLinesBeforeStartingAt: anInteger]
						ifFalse: [0]).
	newLines = 0 ifTrue: [self space] ifFalse: [self newLines: newLines] 