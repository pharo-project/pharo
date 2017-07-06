testing
hasAbstractMethods
	"Tells whether the receiver locally defines an abstract method, i.e., a method sending subclassResponsibility"

	^ super hasAbstractMethods or: [ self theMetaClass hasAbstractMethods ]