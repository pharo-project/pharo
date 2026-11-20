Extension { #name : 'UnhandledError' }

{ #category : '*Debugging-Core' }
UnhandledError >> completeProcess: aProcess with: aContext [
	"The debugger shows the exception that was not handled, not me"

	^ exception completeProcess: aProcess with: aContext
]
