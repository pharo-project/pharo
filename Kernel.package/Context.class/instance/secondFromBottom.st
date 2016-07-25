query
secondFromBottom
	"Return the second from bottom of my sender chain"

	self sender ifNil: [ ^ nil ].
	^ self findContextSuchThat: [ :context |
		context sender sender isNil]