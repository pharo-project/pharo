removing
remove: anObject ifAbsent: aBlock

	| list hash size |
	anObject ifNil: [
		includesNil ifTrue: [
			includesNil := false.
			tally := tally - 1.
			^anObject ].
		^aBlock value ].
	list := (array at: (hash := anObject largeIdentityHash \\ 4096 + 1)) ifNil: [ ^aBlock value ].
	(list fuelPointsTo: anObject) ifFalse: [ ^aBlock value ].
	1 to: (size := tallies at: hash) do: [ :index |
		(list at: index) == anObject 	ifTrue: [
			list
				at: index put: (list at: size);
				at: size put: nil.
			(size := size - 1) < (list size // 4) ifTrue: [
				| newList |
				newList := Array new: size * 2.
				newList
					replaceFrom: 1
					to: size
					with: list
					startingAt: 1.
				array at: hash put: newList ].
			tallies at: hash put: size.
			tally := tally - 1.
			^anObject ] ].
	^aBlock value