testing
hasSession
	"Return if there currently is a server session.
	This only returns a value during #handleRequest:"

	^ ZnCurrentServerSession value notNil