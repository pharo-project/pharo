"
I override some methods in my super to generate instructions for effect and value only.
"
Class {
	#name : #OCASTTranslatorForValue,
	#superclass : #OCASTTranslator,
	#category : #'OpalCompiler-Core-Translator'
}

{ #category : #'inline messages' }
OCASTTranslatorForValue >> emitIfFalse: aMessageNode [
	
	self visitNode: aMessageNode receiver.
	methodBuilder jumpAheadTo: #false if: false.
		methodBuilder pushLiteral: nil.
		methodBuilder jumpAheadTo: #end.
		methodBuilder jumpAheadTarget: #false.
	self visitInlinedBlockNode: aMessageNode arguments first.
	methodBuilder jumpAheadTarget: #end.
]

{ #category : #'inline messages' }
OCASTTranslatorForValue >> emitIfNotNil: aMessageNode [
	| args |
	self visitNode: aMessageNode receiver.
	args := aMessageNode arguments.
	args first arguments ifNotEmpty: [ args first arguments first binding emitStore: methodBuilder ].
	methodBuilder pushDup.
	methodBuilder pushLiteral: nil.
	methodBuilder send: #==.
	methodBuilder jumpAheadTo: #end if: true.
	methodBuilder popTop.
	self visitInlinedBlockNode: args first.
	methodBuilder jumpAheadTarget: #end.
]

{ #category : #'inline messages' }
OCASTTranslatorForValue >> emitIfTrue: aMessageNode [
	
	self visitNode: aMessageNode receiver.
	methodBuilder jumpAheadTo: #else if: false.
	self visitInlinedBlockNode:  aMessageNode arguments first.
	methodBuilder jumpAheadTo: #end.
	methodBuilder jumpAheadTarget: #else.
	methodBuilder pushLiteral: nil.
	methodBuilder jumpAheadTarget: #end.
]

{ #category : #'inline messages' }
OCASTTranslatorForValue >> emitWhileFalse: aMessageNode [

	super emitWhileFalse: aMessageNode.
	methodBuilder pushLiteral: nil.

]

{ #category : #'inline messages' }
OCASTTranslatorForValue >> emitWhileTrue: aMessageNode [

	super emitWhileTrue: aMessageNode.
	methodBuilder pushLiteral: nil.

]

{ #category : #'visitor-double dispatching' }
OCASTTranslatorForValue >> visitSequenceNode: aSequenceNode [ 
	| statements |
	statements := aSequenceNode statements.
	statements ifEmpty: [
		methodBuilder pushLiteral: nil.
		^self].
	statements allButLastDo: [:each | effectTranslator visitNode: each].
	self visitNode: statements last.
]
