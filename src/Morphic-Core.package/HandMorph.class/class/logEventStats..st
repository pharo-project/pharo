events-processing
logEventStats: evt
	EventStats ifNotNil: [
		EventStats at: #count put: (EventStats at: #count ifAbsent:[0]) + 1.
		EventStats at: evt type put: (EventStats at: evt type ifAbsent:[0]) + 1.
		].