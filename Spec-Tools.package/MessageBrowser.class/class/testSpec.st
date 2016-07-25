specs
testSpec
	<spec: #default>
	
	^ SpecLayout composed
			newColumn: [:col |
				col 
					add: #listModel;
					addSplitter;
					add: #toolbarModel height: self buttonHeight;
					add: #textModel ]
				