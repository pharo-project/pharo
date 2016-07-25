searching
matchesAnyArgumentOf: stringCollection do: aBlock 
	stringCollection do: [:each | self matchesArgument: each do: aBlock]