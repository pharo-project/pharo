testing
isHealthy
	"Since this dictionary does no hashing, we consider it healthy
	if it contains no duplicate keys."

	| uniqueKeys |
	uniqueKeys := self setClass new: self size.
	keys
		do: [ :each | 
			each
				ifNotNil: [
					(uniqueKeys includes: each)
						ifTrue: [ ^ false ].
					uniqueKeys add: each ] ].
	^ true