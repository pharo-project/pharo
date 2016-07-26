adding
add: anObject

	| hash |
	anObject ifNil: [ 
		includesNil ifFalse: [
			includesNil := true.
			tally := tally + 1 ].
		^anObject ].
	(array at: (hash := anObject largeIdentityHash \\ 4096 + 1))
		ifNil: [ 
			array at: hash put: (Array with: anObject).
			tallies at: hash put: 1.
			tally := tally + 1 ]
		ifNotNil: [ :list | 
			(list fuelPointsTo: anObject) ifFalse: [
				| newIndex |
				tally := tally + 1.
				list size < (newIndex := (tallies at: hash) + 1)
					ifFalse: [ list at: newIndex put: anObject ]
					ifTrue: [
						| newList |
						newList := Array new: list size * 2.
						newList
							replaceFrom: 1 
							to: list size
							with: list
							startingAt: 1;
							at: newIndex put: anObject.
						array at: hash put: newList ].
				tallies at: hash put: newIndex ] ].
	^anObject