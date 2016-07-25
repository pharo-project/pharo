accessing
session
	"Return the current server session.
	If necessary a new session is created.
	This only returns a value during #handleRequest:"

	^ super session
		ifNil: [ 
			| session |
			session := self server sessionFor: self.
			ZnCurrentServerSession value: session.
			session ]