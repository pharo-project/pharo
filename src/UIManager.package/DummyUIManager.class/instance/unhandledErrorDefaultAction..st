default actions
unhandledErrorDefaultAction: anException

	Transcript show: '*** EXCEPTION ***'; cr.
	Transcript show: anException; cr.
	anException signalerContext errorReportOn: Transcript.
	Smalltalk exitFailure