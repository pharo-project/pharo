queries
isTranslucentButNotTransparent
	"Answer true if this any of this morph is translucent but not transparent."
	
	^ self isTranslucent and: [ self isTransparent not ]