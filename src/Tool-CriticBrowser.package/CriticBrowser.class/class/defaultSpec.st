specs
defaultSpec
	<spec>
	
	^ SpecLayout composed 
		newColumn: [:c |
			c 
				newRow: [:r |
					r 
						newColumn: [:c1 |
							c1 add: #rulesModel.
							c1 newRow: [:c2 |
								c2 add: #resetButton.
								c2 add: #logButton
							] height: 25
						];
						newColumn: [:c1 |
							c1 
								add: #(criticModel textInputFieldModel) height: 20;
								add: #(criticModel criticsModel);
								add: #(criticModel toolbarModel) height: 25 ]];
				addSplitter;
				add: #(criticModel textModel) ]