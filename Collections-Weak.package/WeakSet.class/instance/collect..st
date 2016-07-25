enumerating
collect: aBlock

	| newSet |
	newSet := self species new: self size.
	tally = 0 ifTrue: [ ^newSet ].
	1 to: array size do: [ :index |
		(array at: index) ifNotNil: [ :object |
			object == flag ifFalse: [
				newSet add: (aBlock value: object enclosedSetElement) ] ] ].
	^newSet