"
An UTF8DecomposedTextConverter converts from decomposed UTF8 using the UnicodeCompositionStream.

Instance Variables

"
Class {
	#name : #UTF8DecomposedTextConverter,
	#superclass : #UTF8TextConverter,
	#instVars : [
		'combinedChar'
	],
	#category : #'Multilingual-TextConversion-TextConverter'
}

{ #category : #utilities }
UTF8DecomposedTextConverter class >> encodingNames [

	^Array new

]

{ #category : #conversion }
UTF8DecomposedTextConverter >> nextFromStream: aStream [

	| char resultChar |
	char := super nextFromStream: aStream.
	(CombinedChar isCompositionCharacter: char charCode)
		ifFalse: [^char].
	combinedChar
		ifNil: [
			combinedChar := CombinedChar new.
			combinedChar simpleAdd: char]
		ifNotNil: [
			[combinedChar simpleAdd: char]
				whileFalse: [char := super nextFromStream: aStream].
			resultChar := combinedChar combined.
			combinedChar := CombinedChar new.
			combinedChar simpleAdd: char.
			^resultChar]

]
