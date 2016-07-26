specs
defaultSpec
	<spec>
	
	^ SpecLayout composed
		newColumn: [:c | 
				c 
					newRow: [:r | r 
									add: #changesListModel;
									addSplitter;
									add: #classesListModel ];
					addSplitter;
					add: #methodsListModel;
					addSplitter;
					add: #textModel ].