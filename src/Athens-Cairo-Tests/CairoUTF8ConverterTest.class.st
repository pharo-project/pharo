"
A CairoUTF8ConverterTest is a test class for testing the behavior of CairoUTF8Converter
"
Class {
	#name : #CairoUTF8ConverterTest,
	#superclass : #TestCase,
	#instVars : [
		'encoder'
	],
	#category : #'Athens-Cairo-Tests-Text'
}

{ #category : #running }
CairoUTF8ConverterTest >> setUp [
	super setUp.
	encoder := CairoUTF8Converter new
]

{ #category : #tests }
CairoUTF8ConverterTest >> testEncodingString [
	|  pos |
	pos := encoder convert: 'abc' from: 1 to: 3.

	self assert: pos equals: 3.
	self assert: (encoder buffer first:3) equals: #[97 98 99]
]

{ #category : #tests }
CairoUTF8ConverterTest >> testReusingConverterShouldReplaceOldData [
	| buf |
	buf := encoder convertChar: 223 asCharacter.
	self assert: (buf first: 3) equals: #[195 159 0].
	buf := encoder convertChar: $b.
	self assert: (buf first: 2) equals: #[98 0]
]

{ #category : #tests }
CairoUTF8ConverterTest >> testUnicodencodingShouldTerminateWithNull [
	| buf |
	buf := encoder convertChar: $a.
	self assert: (buf first: 2) equals: #[ 97 0 ]
]
