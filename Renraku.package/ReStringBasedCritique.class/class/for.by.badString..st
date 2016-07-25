as yet unclassified
for: anEntity by: aRule badString: aString
	^ self basicNew
		initializeRule: aRule target: anEntity badString: aString;
		yourself 