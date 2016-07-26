printing
storeString
	"Answer a String representation of the receiver from which the receiver 
	can be reconstructed."

	^ String streamContents: [:s | self storeOn: s]