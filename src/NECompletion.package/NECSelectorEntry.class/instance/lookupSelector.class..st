private
lookupSelector: aSymbol class: aClass 
	"Look up the given selector in my methodDictionary.
	Return the corresponding method if found.
	Otherwise chase the superclass chain and try again.
	Return nil if no method is found."
	| lookupClass |
	lookupClass := aClass.
	[lookupClass isNil]
		whileFalse: [(lookupClass includesSelector: aSymbol)
				ifTrue: [^ Array
						with: lookupClass
						with: (lookupClass compiledMethodAt: aSymbol)].
			lookupClass := lookupClass superclass].
	^ nil