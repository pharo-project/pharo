tests - dictionnary enumerating
testAssociationsDo

	| collection keys |
	collection := self nonEmptyDict .

	keys := OrderedCollection new.
	
	collection associationsDo: [ :assoc | 
		keys add: assoc key.
		self assert: ( collection at: assoc key ) = assoc value.		
		].
	
	collection keys do: [:key | self assert: ( keys occurrencesOf: key ) = (collection keys occurrencesOf: key)].