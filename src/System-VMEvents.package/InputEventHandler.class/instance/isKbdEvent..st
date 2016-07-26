events
isKbdEvent: bufEvt
	^ bufEvt first = EventTypeKeyboard and: [ bufEvt fourth = EventKeyChar ]