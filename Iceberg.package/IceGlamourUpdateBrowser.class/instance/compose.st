building
compose
	self tabulator with: [ :browser |
		browser
			row: [ :top | top
				column: #commits; 
				column: #changes ];
			row: #diff.

		browser transmit
			to: #commits;
			andShow: [ :a | a fastTable
				title: 'New commits to be imported';
				display: #incommingCommits;
				column: 'User' evaluated: #username width: 120;
				column: 'Date & time' evaluated: [:commit | commit datetime asLocal asStringYMDHM ] width: 120;
				column: 'Comment' evaluated: #comment;
				updateOn: IcePackageLoaded from: #announcer;
				addAction: (GLMGenericAction new
					action: [ :presentation :repository | 
						repository command: 'pull'; refresh.
						presentation update ];
					icon: GLMUIThemeExtraIcons glamorousLeftSide;
					shortcut: $f;
					title: 'Fecth changes from upstream'; showTitle);
				addSelectionAction: (GLMGenericAction new
					action: [ :presentation | presentation selection load. presentation selection: nil ];
					icon: GLMUIThemeExtraIcons glamorousLeftSide;
					shortcut: $u;
					title: 'Update to selected commit'; showTitle)
			].

		browser transmit
			fromOutsideEntityPort;
			from: #commits; 
			to: #changes; 
			transformed: [ :model :commit | commit ifNotNil: [model workingCopyDiffToCommit: commit] ];
			andShow: [ :a | a custom: IceGlamourChangesTree new ].

		self addDiffTo: browser withTitle: 'Left: working copy / Right: incomming updates'.
	]
