actions
perform

	| results |
	results := OrderedCollection new.
	actions do: [:each |
		each value ifNotNil: [ :res |  results addAll: res ]
	].
	^ results
