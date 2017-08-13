initialization
obsolete
	"Invalidate and recycle local methods,
	e.g., zap the method dictionary if can be done safely."
	self canZapMethodDictionary
		ifTrue: [self methodDict: self emptyMethodDictionary].
	self hasTraitComposition ifTrue: [
		self traitComposition traits do: [:each |
			each removeUser: self]]