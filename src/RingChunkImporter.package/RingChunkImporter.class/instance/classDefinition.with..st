private
classDefinition: aString with: chgRec
	| tokens theClass |
	
	tokens := self scannedSource: aString.
	theClass := self classNamed: tokens third.
	theClass superclassName: tokens first;
			 definitionSource: aString;
			 category: tokens last.
	
	tokens size = 11
	ifTrue:[ theClass instanceVariables: (tokens fifth findTokens: ' ');
			 	classVariables: (tokens seventh findTokens: ' ');
				 sharedPools: (tokens ninth findTokens: ' ') ].
	
	tokens size = 13
	ifTrue:[ theClass traitCompositionSource: (tokens fifth);  
				instanceVariables: (tokens seventh findTokens: ' ');
			 	classVariables: (tokens ninth findTokens: ' ');
				 sharedPools: ((tokens at: 11) findTokens: ' ') ].