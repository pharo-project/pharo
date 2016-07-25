*NautilusRefactoring
oldVersionTextToDisplay
	| class |
	
	class := Smalltalk 
				at: className asSymbol
				ifAbsent: [ ^ super oldVersionTextToDisplay ].
				
	isMeta ifTrue: [ class := class theMetaClass ].
				
	^ class methodDict 
			at: self selector
			ifPresent: [:method | method sourceCode ]
			ifAbsent: [ super oldVersionTextToDisplay ]