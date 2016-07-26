caching
releaseCachedState
	"Release cached state of the receiver"

	(model ~~ self and: [model respondsTo: #releaseCachedState]) ifTrue:
		[model releaseCachedState].
	super releaseCachedState