api
accept: aText notifying: aNotifyier
	| accept |
	
	accept := actionToPerformHolder value cull: aText cull: aNotifyier.
	accept == false ifTrue: [ ^ false ].
	
	textHolder value: aText.
	
	"Dummy changed to raise an announcement"
	actionPerformedHolder value: aText.
	
	self changed: #clearUserEdits.
	^ true