testing
modifiesClassVariables
	"Note: The meta class has the same class variables as it's non-meta class"

	^ target isInstanceSide and: [ target classVariables ~= classVariables ]