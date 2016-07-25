private
traitDefinition: aString with: chgRec

	| tokens trait |
	tokens := self scannedSource: aString.
	trait := self classNamed: tokens third.
	trait isTrait
		ifFalse: [ trait:= self createTraitNamed: tokens third ].
	trait superclassName: tokens first;
			 definitionSource: aString;
			 category: tokens last