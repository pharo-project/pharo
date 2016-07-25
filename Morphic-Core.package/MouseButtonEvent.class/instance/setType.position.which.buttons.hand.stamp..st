private
setType: evtType position: evtPos which: button buttons: evtButtons hand: evtHand stamp: stamp
	type := evtType.
	position := evtPos.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	whichButton := button.
	timeStamp := stamp.