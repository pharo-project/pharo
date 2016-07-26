removing
removeKey: key ifAbsent: aBlock

	| hash |
	(keys at: (hash := key largeIdentityHash \\ 4096 + 1)) ifNotNil: [ :keyList |
		| size |
		1 to: (size := tallies at: hash) do: [ :index |
			(keyList at: index) == key ifTrue: [
				| valueList |
				keyList
					at: index put: (keyList at: size);
					at: size put: nil.
				(valueList := values at: hash)
					at: index put: (valueList at: size);
					at: size put: nil.
				(size := size - 1) < (keyList size // 4) ifTrue: [
					| newList |
					newList := Array new: size * 2.
					newList
						replaceFrom: 1
						to: size
						with: keyList
						startingAt: 1.
					keys at: hash put: newList.
					newList := Array new: size * 2.
					newList
						replaceFrom: 1
						to: size
						with: valueList
						startingAt: 1.
					values at: hash put: newList ].
				tallies at: hash put: size.
				tally := tally - 1.
				^key ] ] ].
	^aBlock value