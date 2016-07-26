matching
matchesCompletely: anEventBuffer
	
	^ shortcuts anySatisfy: [ :each | each matchesCompletely: anEventBuffer ]