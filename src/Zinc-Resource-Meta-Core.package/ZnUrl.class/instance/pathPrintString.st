printing
pathPrintString
	^ String streamContents: [ :stream | 
			self printPathOn: stream ]