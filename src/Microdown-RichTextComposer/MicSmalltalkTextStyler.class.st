"
I am a smalltalk code styler which renders undefined variables and methods same color as defined.
I am used to render smalltalk codeblocks.
"
Class {
	#name : #MicSmalltalkTextStyler,
	#superclass : #SHRBTextStyler,
	#category : #'Microdown-RichTextComposer-Composer'
}

{ #category : #attributes }
MicSmalltalkTextStyler class >> initialTextAttributesForPixelHeight: aNumber [
	"Render undefined same way as defined"
	"Perhaps this is not the right spot to hook into stuff, but it seems to work"
	| dict |
	dict := super initialTextAttributesForPixelHeight: aNumber.
	dict at: #undefinedIdentifier put: (dict at: #instVar).
	dict at: #undefinedKeyword put: (dict at: #keyword).
	dict at: #undefinedBinary put: (dict at: #binary).
	dict at: #undefinedUnary put: (dict at: #unary).
	^ dict
]

{ #category : #private }
MicSmalltalkTextStyler >> resolveStyleFor: aVariableNode [
	"This circumvent an error in my super"
	aVariableNode isUndeclaredVariable ifTrue: [ ^#undefinedIdentifier ].
	^ super resolveStyleFor: aVariableNode
]
