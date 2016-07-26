as yet unclassified
for: anEntity by: aRule tree: newAST

	^ self basicNew
		initializeRule: aRule
		target: anEntity
		tree: newAST;
		yourself