system startup
shutDown: isImageQuitting
	"The system is going down"
	Current ifNotNil: [ Current shutDown: isImageQuitting ].
