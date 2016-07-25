notification
notifyResponse: response

	SlReporter showNotification ifTrue: [
		response isSuccess
			ifTrue: [ self notifySuccess: response contents ]
			ifFalse: [ self notifyFailure: response contents ] ]