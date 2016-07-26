removing
removeLiberatedCollectors
	mutex critical: [ | liberated |
		liberated := collectors select: #isLiberated.
		collectors removeAll: liberated ]