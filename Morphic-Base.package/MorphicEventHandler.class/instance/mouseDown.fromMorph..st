events
mouseDown: event fromMorph: sourceMorph 
	"Take double-clicks into account."
	((self handlesClickOrDrag: event) and:[event redButtonPressed]) ifTrue:[
		event hand waitForClicksOrDrag: sourceMorph event: event.
	].
	^self notifyMorphsOfEvent: event ofType: #mouseDown from: sourceMorph