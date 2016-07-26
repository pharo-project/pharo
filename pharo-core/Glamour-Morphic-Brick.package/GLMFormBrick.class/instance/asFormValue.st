brick-structure-converting
asFormValue
	|result|
	result := OrderedCollection new.
	self subbricks do: [ :each |
		each asFormValue ifNotNil: [ :v | result addAll: v asOrderedCollection ] ].
	^ result