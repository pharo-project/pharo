controlling
shortDebugStack
	"Answer a String showing the top ten contexts on my sender chain."

	^ String streamContents: [ :stream | self debugStack: 10 on: stream ]