accessing
name
	"Answer a name for the receiver.  This is used generically in the title of certain inspectors, such as the referred-to inspector, and specificially by various subsystems.  By default, we let the object just print itself out..  "

	self
		deprecated: 'Implement your own domain representation of an object, or use #asString or #printString instead.'
		on: '27 May 2016'
		in: #Pharo6. 
	^ self printString