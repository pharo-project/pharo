scripting
custom: aPresentation 
	"this message sets a custom presentation to the current Transmission. 
	it is to be used within the using: block. 
	this message is particularly useful when you want to set a browser as a presentation" 
	 
	self lastTransmission addPresentation: aPresentation. 
	^ aPresentation