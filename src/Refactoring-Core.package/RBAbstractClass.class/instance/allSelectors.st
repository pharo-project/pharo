method accessing
allSelectors
	| class selectors |
	class := self.
	selectors := Set new.
	[class notNil] whileTrue: 
			[selectors addAll: class selectors.
			class := class superclass].
	^selectors