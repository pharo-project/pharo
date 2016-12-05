api
accept: aText notifying: aNotifyier
	| accept newText |
	
	accept := actionToPerformHolder value cull: aText cull: aNotifyier.
	accept == false ifTrue: [ ^ false ].
	
	newText := aText copy.
	textHolder value: newText.
	
	"Dummy changed to raise an announcement"
	actionPerformedHolder value: newText.
	
	self changed: #clearUserEdits.
	^ true