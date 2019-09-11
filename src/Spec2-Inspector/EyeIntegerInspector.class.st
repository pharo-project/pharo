"
Specialized version of inspector showing hex, octal and binary representations.

If within Unicode range, show a Character having using the Integer value as code point.
"
Class {
	#name : #EyeIntegerInspector,
	#superclass : #EyeInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #list }
EyeIntegerInspector >> addSpecialFields: elements [

	elements add: (DynamicEyeElement 
		host: self object 
		label: 'hex' 
		description: [ self object printStringRadix: 16 ]
		value: [ self object ]).
			
	elements add: (DynamicEyeElement 
		host: self object 
		label: 'octal' 
		description: [ self object printStringRadix: 8 ]
		value: [ self object ]).
			
	elements add: (DynamicEyeElement 
		host: self object 
		label: 'binary' 
		description: [ self object printStringRadix: 2 ]
		value: [ self object ]).
			
	(self object between: 0 and: 16r10FFFF)
		ifTrue: [ 
			elements add: (DynamicEyeElement 
				host: self object 
				label: 'character' 
				description: [ :value | value printString ]
				value: [ self object asCharacter ]) ]
]
