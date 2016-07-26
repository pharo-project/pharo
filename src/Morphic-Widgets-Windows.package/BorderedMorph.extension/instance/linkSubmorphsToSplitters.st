*Morphic-Widgets-Windows
linkSubmorphsToSplitters

	self splitters do:
		[:each |
		each splitsTopAndBottom
			ifTrue:
				[self submorphsDo:
					[:eachMorph |
					(eachMorph ~= each and: [eachMorph layoutFrame bottomFraction = each layoutFrame topFraction]) ifTrue: [each addLeftOrTop: eachMorph].
					(eachMorph ~= each and: [eachMorph layoutFrame topFraction = each layoutFrame bottomFraction]) ifTrue: [each addRightOrBottom: eachMorph]]]
			ifFalse:
				[self submorphsDo:
					[:eachMorph |
					(eachMorph ~= each and: [eachMorph layoutFrame rightFraction = each layoutFrame leftFraction]) ifTrue: [each addLeftOrTop: eachMorph].
					(eachMorph ~= each and: [eachMorph layoutFrame leftFraction = each layoutFrame rightFraction]) ifTrue: [each addRightOrBottom: eachMorph]]]]