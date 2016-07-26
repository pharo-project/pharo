class initialization
initializeEventTypeMap
	"self initializeEventTypeMap"
	EventTypeMap := Dictionary new.
	SDL2MappedEvent  allSubclassesDo: [ :cls |
		| eventType |
		eventType := cls eventType.
		eventType ifNotNil: [ EventTypeMap at: eventType put: cls ] ]
