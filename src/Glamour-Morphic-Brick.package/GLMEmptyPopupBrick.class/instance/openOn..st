popup - opening
openOn: aBrick
	"Open popup for a given brick playing a role of my anchor"

	self
		assert: [ aBrick isNotNil ]
		description: [ 'Can not open below a nil brick!' ].

	self anchorBrick: aBrick.
	self onCreated.
	self announce: #onCreated event: self.

	^ self popupLayout open: self with: (aBrick boundsInWorld expandBy: self inset)