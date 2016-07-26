searching
matchesAnyOf: aStringCollection do: aBlock 
	aStringCollection do: [:each | self matches: each do: aBlock]