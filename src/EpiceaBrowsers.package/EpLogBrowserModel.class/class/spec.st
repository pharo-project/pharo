specs
spec
	<spec: #default>

	^ SpecLayout composed newColumn: [:column |
		column 
			add: #itemsModel origin: 0@0 corner: 1@0.6;
			addSplitter;
			add: #toolbarModel height: self toolbarHeight;
			add: #entryContentModel origin: 0@0.6 corner: 1@1;
			yourself ].