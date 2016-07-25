running
run

	|i|
	i := 1.
	self initializeNodes.
	[i = k] whileFalse: [
		self nodes do: [:n|
			self computeAuthoritiesFor: n].
		self nodes do: [:n|
			self computeHubsFor: n].
		self normalize: self nodes.
"		Transcript cr; show: i; show: ': '.
		self nodes do: [:n|
			Transcript show: n printString]."
	i := i + 1]