building
addExternalProvisions: aCollection
	(aCollection intersection: self externalRequirements)
		do: [:ea | self addProvision: ea]