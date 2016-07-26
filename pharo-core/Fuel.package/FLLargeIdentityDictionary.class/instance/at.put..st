accessing
at: key put: value

	| hash |
	(keys at: (hash := key largeIdentityHash \\ 4096 + 1))
		ifNil: [ 
			keys at: hash put: (Array with: key).
			values at: hash put: (Array with: value).
			tallies at: hash put: 1.
			tally := tally + 1 ]
		ifNotNil: [ :keyList | 
			| newIndex |
			1 to: (tallies at: hash) do: [ :index |
				(keyList at: index) == key ifTrue: [
					^(values at: hash) at: index put: value ] ].
			tally := tally + 1.
			keyList size < (newIndex := (tallies at: hash) + 1)
				ifFalse: [ 
					keyList at: newIndex put: key.
					(values at: hash) at: newIndex put: value ]
				ifTrue: [
					| newList |
					(newList := Array new: keyList size * 2)
						replaceFrom: 1 
						to: keyList size
						with: keyList
						startingAt: 1;
						at: newIndex put: key.
					keys at: hash put: newList.
					(newList := Array new: keyList size * 2)
						replaceFrom: 1 
						to: keyList size
						with: (values at: hash)
						startingAt: 1;
						at: newIndex put: value.
					values at: hash put: newList ].
			tallies at: hash put: newIndex ].
	^value