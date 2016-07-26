accessing
flushAllSuchThat: aBlock
	"Remove from the queue all objects that satisfy aBlock."
	monitor critical: [
		items := items reject: aBlock].
