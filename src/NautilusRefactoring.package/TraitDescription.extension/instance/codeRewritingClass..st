*NautilusRefactoring
codeRewritingClass: aBuilder
	| target |
	target := aBuilder model.
	
	(aBuilder item: #'Rewrite Code')
		action: [ target refactor rewriteCode ];
		parent: #'Code Rewriting';
		order: 0.

	(aBuilder item: #'Search Code')
		action: [ target refactor searchCode ];
		parent: #'Code Rewriting';
		order: 100.
		
	(aBuilder item: #'Type class')
		action: [ target refactor typeClass ];
		parent: #'Code Rewriting';
		order: 200;
		withSeparatorAfter.
		
	(aBuilder item: #'Category Regex')
		action: [ target refactor categoryRegex ];
		parent: #'Code Rewriting';
		order: 300.
		
	(aBuilder item: #'Class Regex')
		action: [ target refactor classRegex ];
		parent: #'Code Rewriting';
		order: 400.
		
	(aBuilder item: #'Protocol Regex')
		action: [ target refactor protocolRegex ];
		parent: #'Code Rewriting';
		order: 500.
		
	(aBuilder item: #'Source Regex')
		action: [ target refactor sourceRegex ];
		parent: #'Code Rewriting';
		order: 600;
		withSeparatorAfter.