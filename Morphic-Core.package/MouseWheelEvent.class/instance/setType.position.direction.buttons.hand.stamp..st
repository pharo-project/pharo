accessing
setType: evtType position: evtPos direction: dirSymbol buttons: evtButtons hand: evtHand stamp: stamp
	"Set the state for the receiver."
	
	type := evtType.
	position := evtPos.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	direction := dirSymbol.
	timeStamp := stamp.