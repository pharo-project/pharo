delivery
activateDeliveryIfAvailable
	^ self canSendDiagnosticsAndUsageData 
		ifTrue: [ self activateDelivery ]
		ifFalse: [ nil ]