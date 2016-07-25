accessing
eventType
	^ self infoAt: #eventType ifAbsent: [ #missing ]