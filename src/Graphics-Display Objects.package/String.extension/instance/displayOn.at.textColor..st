*Graphics-Display Objects
displayOn: aDisplayMedium at: aPoint textColor: aColor
	"Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, rendering the text in the designated color"
	aDisplayMedium getCanvas drawString: self from: 1 to: self size at: aPoint font: nil color: aColor
