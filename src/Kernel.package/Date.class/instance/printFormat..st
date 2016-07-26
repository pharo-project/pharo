printing
printFormat: formatArray 
	"Answer a String describing the receiver using the argument formatArray."

	^ String new: 16 streamContents: [ :aStream |
		self printOn: aStream format: formatArray ]