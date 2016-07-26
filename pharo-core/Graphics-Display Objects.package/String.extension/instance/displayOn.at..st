*Graphics-Display Objects
displayOn: aDisplayMedium at: aPoint 
	"Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, using black-colored text."

	self displayOn: aDisplayMedium at: aPoint textColor: Color black