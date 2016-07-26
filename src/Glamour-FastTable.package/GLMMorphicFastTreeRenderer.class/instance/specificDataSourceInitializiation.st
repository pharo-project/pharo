initialization
specificDataSourceInitializiation
	| presentation |
	super specificDataSourceInitializiation.
	presentation := tableModel glamourPresentation.
	tableModel
		childrenBlock: presentation children;
		maxDepth: presentation maxDepth;
		searchStrategy: presentation strategy