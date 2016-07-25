updating
changed
	"Quicker to invalidate handles individually if target is large (especially the world)"

	self extent > (200@200)
		ifTrue: [(target notNil and: [target ~~ self world]) ifTrue:
					["Invalidate 4 outer strips first, thus subsuming separate damage."
					(self fullBounds areasOutside: target bounds) do:
						[:r | self invalidRect: r]].
				self submorphsDo: [:m | m changed]]
		ifFalse: [super changed].
