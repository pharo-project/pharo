converting
asMorph
	| morphs |
	morphs := OrderedCollection new.
	
	morphs add: self icon asMorph.
	
	morphs add: 
		(ButtonModel new
			label: 'See ', browser view pageSize asString, ' more';
			action: [ self fetchMore ];
			buildWithSpec).

	morphs add: 
		(ButtonModel new
			label: 'See all';
			action: [ self fetchAll ];
			buildWithSpec).
	
	^ EpMorphFactory default rowWithAll: morphs