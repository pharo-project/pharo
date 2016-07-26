printing
pathQueryFragmentPrintString
	^ String streamContents: [ :stream | 
			self printPathQueryFragmentOn: stream ]