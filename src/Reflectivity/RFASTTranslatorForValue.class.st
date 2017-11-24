"
WORKAROUND: copy of OCASTTranslatorForValue.

Will be removed when the new Translor design is in place
"
Class {
	#name : #RFASTTranslatorForValue,
	#superclass : #RFASTTranslator,
	#category : #'Reflectivity-Compiler'
}

{ #category : #'inline messages' }
RFASTTranslatorForValue >> emitIfFalse: aMessageNode [
	
	self visitNode: aMessageNode receiver.
	methodBuilder jumpAheadTo: #false if: false.
		methodBuilder pushLiteral: nil.
		methodBuilder jumpAheadTo: #end.
		methodBuilder jumpAheadTarget: #false.
	self visitInlinedBlockNode: aMessageNode arguments first.
	methodBuilder jumpAheadTarget: #end.
]

{ #category : #'inline messages' }
RFASTTranslatorForValue >> emitIfNotNil: aMessageNode [
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
RFASTTranslatorForValue >> emitIfTrue: aMessageNode [
	
	self visitNode: aMessageNode receiver.
	methodBuilder jumpAheadTo: #else if: false.
	self visitInlinedBlockNode:  aMessageNode arguments first.
	methodBuilder jumpAheadTo: #end.
	methodBuilder jumpAheadTarget: #else.
	methodBuilder pushLiteral: nil.
	methodBuilder jumpAheadTarget: #end.
]

{ #category : #'inline messages' }
RFASTTranslatorForValue >> emitWhileFalse: aMessageNode [

	super emitWhileFalse: aMessageNode.
	methodBuilder pushLiteral: nil.

]

{ #category : #'inline messages' }
RFASTTranslatorForValue >> emitWhileTrue: aMessageNode [

	super emitWhileTrue: aMessageNode.
	methodBuilder pushLiteral: nil.

]

{ #category : #'visitor-double dispatching' }
RFASTTranslatorForValue >> visitSequenceNode: aSequenceNode [ 
	| statements |
	self emitMetaLinkBefore: aSequenceNode.
	self emitPrepareLinkAfter: aSequenceNode.
	
	aSequenceNode hasMetalinkInstead
		ifTrue: [ self emitMetaLinkInstead: aSequenceNode ]
		ifFalse: [ 
			statements := aSequenceNode statements.
			statements ifEmpty: [
			methodBuilder pushLiteral: nil.
			^self].
		statements allButLastDo: [:each | effectTranslator visitNode: each].
		self visitNode: statements last.
	].
	self emitMetaLinkAfter: aSequenceNode.
]
