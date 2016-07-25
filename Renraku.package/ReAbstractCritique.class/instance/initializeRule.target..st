initialization
initializeRule: aRule target: anEntity

	self
		initializeRule: aRule
		sourceAnchor: (ReSourceAnchor entity: anEntity)