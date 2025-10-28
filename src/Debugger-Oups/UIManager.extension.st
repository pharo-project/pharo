Extension { #name : 'UIManager' }

{ #category : '*Debugger-Oups' }
UIManager >> handleWarningDebugRequest: aWarningDebugRequest fromDebuggerSystem: anOupsDebuggerSystem [
	"Handle warning notification. Should be overidden by subclass."

	self subclassResponsibility
]

{ #category : '*Debugger-Oups' }
UIManager >> requestDebuggerOpeningFor: anException [

	<debuggerCompleteToSender>
	self logError: anException.

	self class environment
		at: #OupsDebugRequest
		ifPresent: [ :requestClass |
			(requestClass newForException: anException) submit.
			^ self ]
]
