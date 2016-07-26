private
newSessionForRequest: request
	| id session |
	id := self newSessionId.
	session := ZnServerSession new.
	session id: id.
	sessions at: id put: session.
	^ session