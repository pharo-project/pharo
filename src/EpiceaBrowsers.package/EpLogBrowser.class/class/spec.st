specs
spec
	<spec: #default>

	^ SpecLayout composed newColumn: [:column |
		column 
			newRow: [ :row |
				 row 
					add: #statusBarModel ] 
			height: self buttonHeight;
			add: #itemsModel origin: 0@0 corner: 1@0.7;
			addSplitter;
			newRow: #tabManagerModel origin: 0@0.7 corner: 1@1].