query
allReferencesTo: aLiteral 
	"Answer a Collection of all the methods that refer to aLiteral even deeply embedded in literal array."

	"SystemNavigation new allReferencesTo: #+"
	"SystemNavigation new allReferencesTo: Point binding"
	
	| result |
	result := OrderedCollection new.

	self allBehaviorsDo: [ :class |
		| selectors | 
		selectors :=  class thoroughWhichSelectorsReferTo: aLiteral.
		selectors do: [ :selector |
			result addLast: (self createMethodNamed: selector realParent: class) ] ].
	^ result