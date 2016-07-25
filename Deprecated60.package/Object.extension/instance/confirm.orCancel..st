*Deprecated60
confirm: aString orCancel: cancelBlock
	"Put up a yes/no/cancel menu with caption aString. Answer true if  
	the response is yes, false if no. If cancel is chosen, evaluate  
	cancelBlock. This is a modal question--the user must respond yes or no."

	self
		deprecated: 'Use UIManager default confirm:orCancel: instead.'
		on: '05/24/2016'
		in: #Pharo6.
	^ UIManager default confirm: aString orCancel: cancelBlock