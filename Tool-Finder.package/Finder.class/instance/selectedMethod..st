accessing
selectedMethod: aMethod
	"When the method seletion is changed, I send an event to UI"
	selectedMethod := aMethod.
	self triggerEvent: #updateSelectedMethod