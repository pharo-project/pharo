searching
matchesAnyMethodOf: aStringCollection do: aBlock 
	aStringCollection do: [:each | self matchesMethod: each do: aBlock]