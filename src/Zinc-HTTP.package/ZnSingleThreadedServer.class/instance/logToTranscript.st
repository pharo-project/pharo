logging
logToTranscript
	"Setup logging to the Transcript.
	If logging was completely off, turn it on"

	logLevel isZero ifTrue: [ self loggingOn ].
	ZnLogEvent logToTranscript 