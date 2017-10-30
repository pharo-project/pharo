specs
defaultSpec
	<spec>
	
	^ SpecLayout composed
		newColumn: [:c | 
				c 
					newRow: [:r | r 
									add: #changesListPresenter;
									addSplitter;
									add: #classesListPresenter ];
					addSplitter;
					add: #methodsListPresenter;
					addSplitter;
					add: #textPresenter ].