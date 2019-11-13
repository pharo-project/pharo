Extension { #name : #CommandLineUIManager }

{ #category : #'*CodeImportCommandLineHandlers' }
CommandLineUIManager >> syntaxErrorNotificationDefaultAction: aSyntaxErrorNotification [
	"log the syntax notification and print a nicely formatted and colored syntax error on stderr"
	Smalltalk logDuring: [ :logger |
		logger print: aSyntaxErrorNotification; cr.
		aSyntaxErrorNotification signalerContext errorReportOn: logger ].
	
	STCommandLineHandler printCompilerWarning: aSyntaxErrorNotification.
			
	"in noninteractive mode simply quit"
	^ self exitFailure
]
