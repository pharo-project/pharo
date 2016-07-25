private
addRequirements: aCollection for: anObject
	aCollection do: [:ea | self addRequirement: ea for: anObject]