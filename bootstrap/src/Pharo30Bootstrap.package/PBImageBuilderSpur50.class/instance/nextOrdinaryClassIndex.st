as yet unclassified
nextOrdinaryClassIndex
	
	| result |
	nextOrdinaryClassIndex ifNil: [ nextOrdinaryClassIndex := self firstOrdinaryClassIndex ].
	result := nextOrdinaryClassIndex.
	nextOrdinaryClassIndex := nextOrdinaryClassIndex + 1.
	^ result