enumerating
allUnreferencedInstanceVariables
	"Return a list of the instance variables known to the receiver which are not referenced in the receiver or any of its subclasses OR superclasses"

	^ self allInstVarNames reject: [:ivn |
		| definingClass |		
		definingClass := self classThatDefinesInstanceVariable: ivn.
		definingClass withAllSubclasses anySatisfy: [:class |  
				(class whichSelectorsAccess: ivn asSymbol) notEmpty]]