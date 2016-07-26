as yet unclassified
hasCategories: aCollection
	|counter|
	candidates processors asArray size = aCollection size ifFalse: [ ^ false ].
	counter := 1.
	candidates processors asArray
		do: [ :each | (each = (aCollection at: counter)) ifFalse: [ ^ false ] . counter := counter + 1].
	^ true