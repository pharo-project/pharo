accessing
entryBufferDo: aBlock
	"Iterate the entryBuffer taking care of performance (avoid triggering the lazy initialization)."

	entryBuffer ifNotNil: [
		"Create a new Array instance on purpose, to avoid any possible concurrency issue, since the original collection may mutate buring the iteration."
		entryBuffer asArray do: aBlock ]