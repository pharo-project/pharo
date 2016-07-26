testing
hasAbstractMethods
	"Tells whether the receiver locally defines an abstract method, i.e., a method sending subclassResponsibility"
	
	^ (self methods anySatisfy: [:cm | cm sendsSelector: #subclassResponsibility ])