drawing
drawDuring: aBlock

	"You may draw on receiver only when inside a block and only using provided canvas object.
	This ensures releasing system resources used after finishing drawing"

	self checkSession.
	
	currentCanvas ifNotNil: [ self attemptToRecurseDrawing ].
	[
		currentCanvas := context.
		self privSetDefaults.
		aBlock value: currentCanvas.
		self flush.
	] ensure: [
		currentCanvas := nil.
	].