protocol
enabled: aBoolean
	"Pass on to submorphs."

	self submorphsDo: [:m | m enabled: aBoolean]