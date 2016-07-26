*Morphic-Base
currentEvent
	"Answer the current Morphic event.  This method never returns nil."
	^ActiveEvent ifNil:[self currentHand lastEvent]