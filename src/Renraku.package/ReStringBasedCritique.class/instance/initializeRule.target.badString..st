initialization
initializeRule: aRule target: anEntity badString: aString

	self
		initializeRule: aRule
		sourceAnchor: (
			ReSearchStringSourceAnchor
				entity: anEntity
				string: aString)