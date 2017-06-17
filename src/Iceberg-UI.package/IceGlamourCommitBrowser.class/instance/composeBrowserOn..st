building
composeBrowserOn: browser
	browser
		row: [ :row | 
			row
				column: #changes; 
				column: #commit ];
		row: #diff.

	browser transmit 
		to: #changes; 
 		andShow: [ :a :diff |
			(IceDiffChangeTreeBuilder new 
				diff: diff;
				buildOn: a)
				title: 'Local changes' ].

	browser transmit 
		to: #commit; 
		andShow: [ :a :diff | 
			a morph
				title: 'Commit changes';
				morph: [
					IceCommitModel new 
						model: diff;
						buildWithSpec ] ].

	self addDiffTo: browser withTitle: 'Left: working copy / Right: local repository'