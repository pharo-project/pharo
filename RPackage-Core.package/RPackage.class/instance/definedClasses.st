accessing
definedClasses 
	| definedClasses |
	definedClasses := Set new.
	classes do: [:each | 
		Smalltalk globals 
			at: each 
			ifPresent: [:aClass| definedClasses add: aClass ]].
	^ definedClasses
	