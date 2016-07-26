instance creation
extent: anExtent format: aFormat
	^ ( self primImage: aFormat width: anExtent x height: anExtent y + 1) initialize
