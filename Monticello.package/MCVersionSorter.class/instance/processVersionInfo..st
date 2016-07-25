private
processVersionInfo: aVersionInfo
	(self addToCurrentLayer: aVersionInfo) ifTrue:
		[self pushLayer.
		(self knownAncestorsOf: aVersionInfo) do: [:ea | self processVersionInfo: ea].
		self popLayer]
