responses
status: request
	"Reply with a dynamic HTML page describing the state of the server"

	^ ZnResponse ok: (ZnEntity html: self generateStatus)
	