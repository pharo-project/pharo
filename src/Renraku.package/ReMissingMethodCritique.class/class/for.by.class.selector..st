as yet unclassified
for: anEntity by: aRule class: aClass selector: aSymbol

	^ self basicNew 
		initializeRule: aRule target: anEntity class: aClass selector: aSymbol;
		yourself 