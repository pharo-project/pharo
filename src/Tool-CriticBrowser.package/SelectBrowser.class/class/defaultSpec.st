specs
defaultSpec

		<spec>
	
	^ SpecLayout composed
		newColumn: [:c |
			c
			newRow: [:r |
				r 
					add: #toSelectModel;
					addSplitter;
					add:  #selectedModel];
			newRow: [:r |
				r 
					add:#addButtonModel;
					add:#addAllButtonModel;
					add:#removeButtonModel;
					add:#removeAllButtonModel;
					add: #nextButtonModel ] height: 25]	