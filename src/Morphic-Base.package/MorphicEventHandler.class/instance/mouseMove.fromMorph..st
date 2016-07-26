events
mouseMove: event fromMorph: sourceMorph
	^self notifyMorphsOfEvent: event ofType: #mouseMove from: sourceMorph