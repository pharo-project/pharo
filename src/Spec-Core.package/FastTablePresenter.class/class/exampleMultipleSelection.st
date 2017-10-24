example
exampleMultipleSelection
	| example |
	example := self new.
	example
		items: (1 to: 10);
		beMultipleSelection;
		whenSelectionChanged: [ :selection | selection logCr ];
		openWithSpec.
	^ example