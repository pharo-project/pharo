"
I am a parse tree leaf representing a literal variable.
"
Class {
	#name : #LiteralVariableNode,
	#superclass : #VariableNode,
	#instVars : [
		'writeNode'
	],
	#category : 'Compiler-ParseNodes'
}

{ #category : #visiting }
LiteralVariableNode >> accept: aVisitor [
	^aVisitor visitLiteralVariableNode: self
]

{ #category : #'code generation' }
LiteralVariableNode >> emitCodeForLoad: stack encoder: encoder [
	writeNode ifNotNil:
		[encoder genPushLiteral: index.
		 stack push: 1]
]

{ #category : #'code generation' }
LiteralVariableNode >> emitCodeForStore: stack encoder: encoder [
	writeNode ifNil: [^encoder genStoreLiteralVar: index].
	"THIS IS WRONG!! THE VALUE IS LOST FROM THE STACK!!
	 The various value: methods on Association ReadOnlyVariableBinding
	 etc _do not_ return the value assigned; they return the receiver."
	"Should generate something more like
		push expr
		push lit
		push temp (index of expr)
		send value:
		pop
	or use e.g. valueForStore:"
	self flag: #bogus.
	writeNode
		emitCode: stack
		args: 1
		encoder: encoder
		super: false
]

{ #category : #'code generation' }
LiteralVariableNode >> emitCodeForStorePop: stack encoder: encoder [
	writeNode ifNil:
		[stack pop: 1.
		 ^encoder genStorePopLiteralVar: index].
	self emitCodeForStore: stack encoder: encoder.
	encoder genPop.
	stack pop: 1.
]

{ #category : #'code generation' }
LiteralVariableNode >> emitCodeForValue: stack encoder: encoder [
	stack push: 1.
	^encoder genPushLiteralVar: index
]

{ #category : #'code generation' }
LiteralVariableNode >> sizeCodeForLoad: encoder [
	self reserve: encoder.
	^writeNode ifNil: [0] ifNotNil: [encoder sizePushLiteral: index]
]

{ #category : #'code generation' }
LiteralVariableNode >> sizeCodeForStore: encoder [
	self reserve: encoder.
	^encoder sizeStoreLiteralVar: index
	
]

{ #category : #'code generation' }
LiteralVariableNode >> sizeCodeForStorePop: encoder [
	self reserve: encoder.
	^encoder sizeStorePopLiteralVar: index
]

{ #category : #'code generation' }
LiteralVariableNode >> sizeCodeForValue: encoder [
	self reserve: encoder.
	^encoder sizePushLiteralVar: index
]
