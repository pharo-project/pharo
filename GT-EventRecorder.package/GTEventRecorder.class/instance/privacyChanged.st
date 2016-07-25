event handling
privacyChanged
	self canSendDiagnosticsAndUsageData 
		ifTrue: [ self numberOfCollectors > 0 
						ifTrue: [ self activateDelivery ] ]
		ifFalse: [ self deactivateDelivery ]