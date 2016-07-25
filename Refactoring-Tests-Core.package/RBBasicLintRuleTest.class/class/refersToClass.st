miscellaneous
refersToClass
	| detector |
	detector := self new.
	detector name: 'Refers to class name instead of "self class"'.
	detector classBlock: 
			[:context :result | 
			| sels className |
			className := context selectedClass theNonMetaClass name.
			sels := context selectedClass
						whichSelectorsReferTo: (Smalltalk associationAt: className).
			sels isEmpty ifFalse: 
					[result addSearchString: className.
					sels do: [:each | result addClass: context selectedClass selector: each]]].
	^detector