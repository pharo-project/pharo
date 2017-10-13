public
adoptAsDefault: server
	"Adopt server as the default instance that we manage.
	If there was a previous default, stop it.
	Note that this is a class instance variable."
	
	self stopDefault.
	default := server