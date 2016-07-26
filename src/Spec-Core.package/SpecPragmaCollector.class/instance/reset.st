initializing
reset
	"reinitialize current system settings"
	self
		noMoreAnnounceWhile: [self collected copy
				do: [:pragma | self removePragma: pragma].
			self allPragmas
				do: [:pragma | self addPragma: pragma]].
	self announce: (PragmaCollectorReset collector: self)