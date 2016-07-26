building
compose
	self tabulator with: [ :browser | 
		browser
			row: [ :top | top
				column: #changes; 
				column: #commit ];
			row: #diff.

		browser transmit 
			to: #changes; 
			andShow: [ :a | a custom: IceGlamourChangesTree new ].
	
		browser transmit to: #commit; andShow: [ :a | a text
			display: '';
			title: 'Commit message';
			addAction: (GLMGenericAction new
				action: [ :presentation :entity | entity commitWithMessage: presentation text asString ];
				icon: GLMUIThemeExtraIcons glamorousSave;
				shortcut: $s;
				title: 'Commit'; showTitle)
		].

		self addDiffTo: browser withTitle: 'Left: working copy / Right: local repository'.
	]
