*NautilusRefactoring
groupRefactoring: aBuilder
	| target |
	target := aBuilder model.
	
	(aBuilder item: #'Rewrite Code')
		order:  100;
		withSeparatorAfter.
	
	(aBuilder item: #'Rewrite Code')
		action: [ target refactor rewriteCode ];
		parent: #'Rewrite Code';
		order: 0.

	(aBuilder item: #'Search Code')
		action: [ target refactor searchCode ];
		parent: #'Rewrite Code';
		order: 100.