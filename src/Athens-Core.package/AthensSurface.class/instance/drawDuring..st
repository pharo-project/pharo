public
drawDuring: aBlock

	"You may draw on receiver only when inside a block and only using provided canvas object.
	This ensures releasing system resources used after finishing drawing"


	currentCanvas ifNotNil: [ self attemptToRecurseDrawing ].
	
	[
		currentCanvas := self newCanvas.
		aBlock value: currentCanvas.
	] ensure: [
		self releaseCanvas.
		currentCanvas := nil.
	].