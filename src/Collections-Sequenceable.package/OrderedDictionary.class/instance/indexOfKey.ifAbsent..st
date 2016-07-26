accessing
indexOfKey: aKey ifAbsent: aBlock
	1 to: self size do: [:i |
		(orderedKeys at: i) = aKey
			ifTrue: [^ i]].
	^ aBlock value.