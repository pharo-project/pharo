accessing
at: key ifAbsent: aBlock

	| hash |
	(keys at: (hash := key largeIdentityHash \\ 4096 + 1)) ifNotNil: [ :keyList | 
		1 to: (tallies at: hash) do: [ :index |
			(keyList at: index) == key ifTrue: [
				^(values at: hash) at: index ] ] ].
	^aBlock value