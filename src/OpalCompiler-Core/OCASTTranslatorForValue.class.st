"
I override some methods in my super to generate instructions for effect and value only.
"
Class {
	#name : #OCASTTranslatorForValue,
	#superclass : #OCASTTranslator,
	#category : #'OpalCompiler-Core-Translator'
}

{ #category : #'inline messages' }
OCASTTranslatorForValue >> emitWhileFalse: aMessageNode [

	super emitWhileFalse: aMessageNode.
	methodBuilder pushLiteral: nil
]

{ #category : #'inline messages' }
OCASTTranslatorForValue >> emitWhileTrue: aMessageNode [

	super emitWhileTrue: aMessageNode.
	methodBuilder pushLiteral: nil
]
