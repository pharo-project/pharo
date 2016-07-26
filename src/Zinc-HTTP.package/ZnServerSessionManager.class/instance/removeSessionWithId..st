accessing
removeSessionWithId: id
	"Remove the session with id.
	If there is no such session, do nothing"
	
	^ sessions removeKey: id ifAbsent: [ nil ]