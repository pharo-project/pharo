accessing
addPresentationSilently: each
	^ presentations
		add:
			(each
				pane: self;
				yourself)