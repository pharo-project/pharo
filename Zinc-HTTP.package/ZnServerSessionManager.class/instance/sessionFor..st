accessing
sessionFor: request
	"Bind an existing session to request or create a new session"

	| id |
	id := self sessionIdFromRequest: request.
	sessions
		at: id
		ifPresent: [ :session | 
			session isValid
				ifTrue: [ ^ session touch; yourself ]
				ifFalse: [ sessions removeKey: id ] ].
	^ self newSessionForRequest: request