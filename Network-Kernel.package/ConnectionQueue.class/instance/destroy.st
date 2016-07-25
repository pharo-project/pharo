public
destroy
	"Terminate the listener process and destroy all sockets in my possesion."

	process ifNotNil: [
		process terminate.
		process := nil].
	socket ifNotNil: [
		socket destroy.
		socket := nil].
	connections do: [:s | s destroy].
	connections := OrderedCollection new.
