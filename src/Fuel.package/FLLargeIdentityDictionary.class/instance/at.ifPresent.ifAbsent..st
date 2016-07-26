accessing
at: key ifPresent: presentBlock ifAbsent: absentBlock

	| hash |
	(keys at: (hash := key largeIdentityHash + 1)) ifNotNil: [ :keyList | 
		1 to: (tallies at: hash) do: [ :index |
			(keyList at: index) == key ifTrue: [
				^ presentBlock cull: ((values at: hash) at: index) ] ] ].
	^absentBlock value