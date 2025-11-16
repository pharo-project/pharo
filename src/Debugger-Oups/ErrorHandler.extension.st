Extension { #name : 'ErrorHandler' }

{ #category : '*Debugger-Oups' }
ErrorHandler >> handleWarningDebugRequest: aWarningDebugRequest fromDebuggerSystem: anOupsDebuggerSystem [
	"Handle warning notification. Should be overidden by subclass."

	self subclassResponsibility
]
