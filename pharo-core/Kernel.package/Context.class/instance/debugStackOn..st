printing
debugStackOn: aStream
	"print the top ten contexts on my sender chain."
	^ self debugStack: 100 on: aStream