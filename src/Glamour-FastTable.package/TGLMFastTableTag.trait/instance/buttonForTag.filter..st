factory
buttonForTag: each filter: filter
	^ (GLMMorphic togglingButtonLabelled: each pressed: (self shouldFilterByTag: filter) style: self glamourPresentation tagsStyle)
		target: self;
		actionSelector: #toggleFilteringByTag:;
		arguments: (Array with: filter);
		yourself