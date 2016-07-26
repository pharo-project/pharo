adding nodes
addTemporariesNamed: aCollection
	^ aCollection collect: [ :each | self addTemporaryNamed: each ]