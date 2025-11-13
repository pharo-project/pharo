Extension { #name : 'NonInteractiveErrorHandler' }

{ #category : '*Debugger-Oups' }
NonInteractiveErrorHandler >> handleWarningDebugRequest: aDebugRequest fromDebuggerSystem: anOupsDebuggerSystem [

	anOupsDebuggerSystem warningRequest: aDebugRequest withErrorHandler: self
]
