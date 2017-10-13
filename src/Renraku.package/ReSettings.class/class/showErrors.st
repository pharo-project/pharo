settings-accessing
showErrors
	self flag: 'switch to exception strategy completely'.
	"Utilize the ReExceptionStrategy to either skip the exception,
	pass it on, or create a list of exceptions as a reparete
	'category' with all the exceptions that happened."
	
	^ ReExceptionStrategy current = RePassExceptionStrategy