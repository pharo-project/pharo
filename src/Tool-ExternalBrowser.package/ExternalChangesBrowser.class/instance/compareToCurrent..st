menu
compareToCurrent: aMethodDeclaration

	| class sourceString |
	class := aMethodDeclaration targetClass.
	sourceString := (class >> aMethodDeclaration methodSelector) sourceCode.
	UIManager default
		openComparisonFrom: aMethodDeclaration contents
		to: sourceString
		belongingTo: class
		from: aMethodDeclaration
		labeled: 'Current'
		inWindowLabeled: 'Comparison to Current Version'