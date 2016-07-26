public
adoptAsDefault: server
	"Adopt server as the default instance that we manage.
	If there was a previous default, stop it.
	Delegate to the default server class."
	
	self defaultServerClass adoptAsDefault: server