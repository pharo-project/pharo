accessing
styleAtPosition: txPosition 

	"The receiver's attributes is defined for the next line,
	however a text position pointing to beginning of receiver is formally an insertion point belonging
	to previous line, therefore we must use previous span attributes"
	
	^ self style