testing
isTranslucentButNotTransparent
	"Answer true if this any of this morph is translucent but not transparent."

	(color isColor and: [color isTranslucentButNotTransparent]) ifTrue: [^ true].
	(borderColor isColor and: [borderColor isTranslucentButNotTransparent]) ifTrue: [^ true].
	^ false
