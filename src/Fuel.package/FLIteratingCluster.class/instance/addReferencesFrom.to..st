analyzing
addReferencesFrom: anObject to: aCollection
	aCollection do: [ :ref |
		self addReferenceFrom: anObject to: ref ]