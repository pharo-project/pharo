"
I am  EyeCharacterInspector.
I am a EyeInspector.

I offer a specialized inspector to look at Character instances. 

I show the Unicode code point in standard notation, like U+0041 for $A.

"
Class {
	#name : #SpEyeCharacterInspector,
	#superclass : #SpEyeInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #list }
SpEyeCharacterInspector >> addSpecialFields: elements [

	elements add: (SpDynamicEyeElement 
		host: self object 
		label: 'unicode' 
		description: [ self objectAsUnicodeCodePoint ]
		value: [ self object codePoint ])
]

{ #category : #converting }
SpEyeCharacterInspector >> objectAsUnicodeCodePoint [
	^ String streamContents: [ :stream | 
			stream << 'U+'.
			self object codePoint 
				printOn: stream base: 16 nDigits: 4 ]
]
