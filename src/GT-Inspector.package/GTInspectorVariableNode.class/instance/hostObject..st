accessing
hostObject: anObject

	"
	Allow nil objects for the moment. Normally the inspector should be able to inspect nil.
	self
		assert: [ anObject isNotNil ]
		description: [ 'Host object must not be nil' ]."

	self
		assert: [ hostObject isNil ]
		description: [ 'Host can be assigned only once' ].

	hostObject := anObject