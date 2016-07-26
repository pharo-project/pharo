initialization
specificTableMorphInitializiation
	| presentation |
	super specificTableMorphInitializiation.
	presentation := tableModel glamourPresentation.
	tableMorph intercellSpacing: tableModel glamourPresentation intercellSpacing.
	tableMorph beResizable.
	tableMorph
		columns:
			(presentation columns
				collect:
					[ :each | 
					GLMFastTableColumn new
						id: (presentation titleValueOfColumn: each);
						width: each width;
						glamourColumn: each;
						target: tableModel actionSelector: #sortByColumn: arguments: {each};
						yourself ]).
	presentation shouldShowHeader 
		ifFalse: [ tableMorph hideColumnHeaders ]
		ifTrue: [ tableMorph showColumnHeaders ]