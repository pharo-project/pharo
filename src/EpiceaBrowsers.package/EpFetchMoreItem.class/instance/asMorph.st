converting
asMorph
	| morphs |
	morphs := OrderedCollection new.
	
	morphs add: self icon asMorph.
	
	morphs add: 
		(ButtonModel new
			label: 'fetch more';
			action: [ self fetchMore ];
			buildWithSpec).
	
	^ self panelWidgetWithAll: morphs