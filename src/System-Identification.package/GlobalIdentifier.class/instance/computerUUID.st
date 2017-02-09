accessing
computerUUID
	"This value identifies a user logged into the computer.
	Before calling #computerUUID method, you should call #ensure."
	^ persistedInformation at: #computerUUID