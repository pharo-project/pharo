printing
shortDebugStackOn: aStream
	"print the top 30 contexts on my sender chain."

	^ self debugStack: 30 on: aStream