correcting
showWarningOnTranscript

	true ifTrue: [ ^ self ]. "turned off by default"

	Transcript crShow: self warningMessage