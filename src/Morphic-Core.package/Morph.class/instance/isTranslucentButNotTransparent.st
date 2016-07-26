testing
isTranslucentButNotTransparent
	"Answer true if this any of this morph is translucent but not transparent."

	^ color isColor and: [ color isTranslucentButNotTransparent ]
