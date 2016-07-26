accessing
flush
	"Remove from the queue all objects"
	monitor critical: [
		items := items removeAll].
