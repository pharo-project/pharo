as yet unclassified
useOKDefaultCancelButton
	"Set the buttons to be an OK button and a default cancel button.
	Only effective before the model is set."
	
	self buttons: {self newOKButton. self newCancelButton isDefault: true}