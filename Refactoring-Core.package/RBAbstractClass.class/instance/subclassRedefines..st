testing
subclassRedefines: aSelector
	"Return true, if one of your subclasses redefines the method with name, aMethod"

	^ self allSubclasses anySatisfy: [ :each | each directlyDefinesMethod: aSelector ]