"
I am  EyeCharacterInspector.
I am a EyeInspector.

I offer a specialized inspector to look at Character instances. 

I show the Unicode code point in standard notation, like U+0041 for $A.

"
Class {
	#name : #EyeCharacterInspector,
	#superclass : #EyeInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #list }
EyeCharacterInspector >> addSpecialFields: elements [

	elements add: (DynamicEyeElement 
		host: self object 
		label: 'unicode' 
		description: [ self objectAsUnicodeCodePoint ]
		value: [ self object codePoint ])
]

{ #category : #converting }
EyeCharacterInspector >> objectAsUnicodeCodePoint [
	^ String streamContents: [ :stream | 
			stream << 'U+'.
			self object codePoint 
				printOn: stream base: 16 nDigits: 4 ]
]
