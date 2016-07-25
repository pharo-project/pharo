accessing
announcer
	"Answer the receiver's announcer, creating if required."
	
	^ announcer ifNil: [ announcer := Announcer new ]