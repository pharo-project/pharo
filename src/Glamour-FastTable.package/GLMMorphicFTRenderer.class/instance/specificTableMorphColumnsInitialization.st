private
specificTableMorphColumnsInitialization
	| presentation |
	presentation := tableModel glamourPresentation.
	tableMorph intercellSpacing: tableModel glamourPresentation intercellSpacing.
	tableMorph beResizable.
	presentation shouldShowHeader 
		ifFalse: [ tableMorph hideColumnHeaders ]
		ifTrue: [ tableMorph showColumnHeaders ].
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