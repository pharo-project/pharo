convenience
logToTranscript
	self stopLoggingToTranscript.
	^ self announcer when: ZnLogEvent do: [ :event | Transcript crShow: event ].