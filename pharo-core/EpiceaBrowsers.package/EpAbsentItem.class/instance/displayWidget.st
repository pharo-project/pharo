populating browser
displayWidget
	| morphs |
	morphs := OrderedCollection new.
	 
	morphs add: self icon asMorph.

	morphs add: 
		(self 
			tagWidgetFor: 'Absent entry'
			color: Color lightBlue).

	morphs add: 
		(ButtonModel new
			label: 'open';
			action: [ self openLogOfAbsentEntry ];
			buildWithSpec).
	
	^ self panelWidgetWithAll: morphs