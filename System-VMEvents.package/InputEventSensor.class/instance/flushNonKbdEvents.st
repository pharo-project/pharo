private events
flushNonKbdEvents
	eventQueue ifNil: [^ self].
	eventQueue flushAllSuchThat:
		[:buf | (self isKbdEvent: buf) not]