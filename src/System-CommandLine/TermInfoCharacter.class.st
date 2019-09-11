"
I'm a TermCap which define the styles with the printing of characters, used when OSSubProcess is not installed in the image.
Not really powerfull, better install OSSubProcess and use TermCapTput
"
Class {
	#name : #TermInfoCharacter,
	#superclass : #NullTermInfo,
	#instVars : [
		'out'
	],
	#category : #'System-CommandLine-Utilities'
}

{ #category : #TermStyle }
TermInfoCharacter >> on: aStream [
	"Define the stream where termcap will change the style, normaly it's used on a terminal"

	out := aStream
]

{ #category : #streaming }
TermInfoCharacter >> print: aString [
	"Print on the outStream aString"

	out nextPutAll: aString
]

{ #category : #TermStyle }
TermInfoCharacter >> set: aString [
	self print: Character escape asString, '['   , aString
]

{ #category : #styling }
TermInfoCharacter >> setBackgroundColor: aColor [

	self set: aColor asTerminfoCharacterBackground , 'm'.
]

{ #category : #styling }
TermInfoCharacter >> setFontColor: aColor [ 

	self set: aColor asTerminfoCharacterFont, 'm'
]
