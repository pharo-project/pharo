callbacks
buttonForTag: each filter: filter
	| button |

	button := GLMMorphic 
		togglingButtonLabelled: each
		pressed: (self containerTree shouldFilterByTag: filter)
		style: self containerTree glamourPresentation tagsStyle.
	button
		target: self;
		actionSelector: #toggleFilteringByTag:;
		arguments: (Array with: filter).
	^ button