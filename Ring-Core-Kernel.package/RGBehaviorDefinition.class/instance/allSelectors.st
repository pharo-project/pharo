accessing methods
allSelectors
	"Retrieves all the selectos of the receiver in the chain hierarchy"
	| class selectors |
	
	class:= self.
	selectors := Set new.
	[class notNil] whileTrue: 
			[selectors addAll: class selectors.
			class := class superclass ].
	^selectors