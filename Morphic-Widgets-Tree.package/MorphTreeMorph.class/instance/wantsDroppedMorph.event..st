dropping/grabbing
wantsDroppedMorph: aMorph event: anEvent
	"Return true if the receiver wishes to accept the given morph, which is being dropped 
	by a hand in response to the given event. Note that for a successful drop operation 
	both parties need to agree. The symmetric check is done automatically
	 via aMorph wantsToBeDroppedInto: self."

	^ self model wantsDroppedMorph: aMorph event: anEvent inMorph: self