as yet unclassified
useDefaultOKButton
	"Set the buttons to be just an OK button.
	Only effective before the model is set."
	
	self buttons: {self newOKButton isDefault: true}