testing
isHealthy
	| uniqueKeys |
	"ensure there are no duplicate keys"
	uniqueKeys := self setClass new: self size.
	keys
		do: [ :each | 
			each
				ifNotNil: [
					(uniqueKeys includes: each)
						ifTrue: [ ^ false ].
					uniqueKeys add: each ] ].
	^ true