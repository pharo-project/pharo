responses
help: request
	"Reply with a dynamic HTML page containing links to all pages I support"

	^ ZnResponse ok: (ZnEntity html: self generateHelp)
	