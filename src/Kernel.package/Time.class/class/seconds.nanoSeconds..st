instance creation
seconds: seconds nanoSeconds: nanoCount
	"Answer a Time from midnight."

	^ self basicNew
		ticks: (Duration seconds: seconds nanoSeconds: nanoCount) ticks