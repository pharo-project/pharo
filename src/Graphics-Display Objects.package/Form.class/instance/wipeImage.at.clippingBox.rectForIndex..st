transitions
wipeImage: otherImage at: topLeft clippingBox: clipBox rectForIndex: rectForIndexBlock

	| i clipRect t rectOrList waitTime |
	i := 0.
	clipRect := topLeft extent: otherImage extent.
	clipBox ifNotNil: [clipRect := clipRect intersect: clipBox ifNone: [ ^ self ]].
	[rectOrList := rectForIndexBlock value: (i := i + 1).
	 rectOrList == nil]
		whileFalse: [
			t := Time millisecondClockValue.
			rectOrList asOrderedCollection do: [:r |
				self copyBits: r from: otherImage at: topLeft + r topLeft
					clippingBox: clipRect rule: Form over fillColor: nil].
			Display forceDisplayUpdate.
			waitTime := 3 - (Time millisecondClockValue - t).
			waitTime > 0 ifTrue:
				["(Delay forMilliseconds: waitTime) wait"]].
