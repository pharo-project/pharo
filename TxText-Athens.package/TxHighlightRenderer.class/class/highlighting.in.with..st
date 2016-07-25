instance creation
highlighting: aTxInterval in: layoutView with: paint

	^ self new
		layoutView: layoutView;
		interval: aTxInterval;
		paint: paint;
		yourself.