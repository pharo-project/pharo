events
mouseUp: event fromMorph: sourceMorph
	^self notifyMorphsOfEvent: event ofType: #mouseUp from: sourceMorph