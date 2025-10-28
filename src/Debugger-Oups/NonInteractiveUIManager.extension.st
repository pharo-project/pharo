Extension { #name : 'NonInteractiveUIManager' }

{ #category : '*Debugger-Oups' }
NonInteractiveUIManager >> handleWarningDebugRequest: aDebugRequest fromDebuggerSystem: anOupsDebuggerSystem [

	anOupsDebuggerSystem warningRequest: aDebugRequest withErrorManager: self
]
