events
mouseLeave: event fromMorph: sourceMorph
	^self notifyMorphsOfEvent: event ofType: #mouseLeave from: sourceMorph