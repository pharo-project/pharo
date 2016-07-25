specs
defaultSpec
	<spec>
	
	^ SpecLayout composed
		newColumn: [:c |
			c 
				add: #criticsModel;
				add: #toolbarModel height: 25;
				addSplitter;
				add: #textModel ]