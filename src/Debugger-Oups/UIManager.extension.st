Extension { #name : 'UIManager' }

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
