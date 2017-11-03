"
I am a parse tree leaf representing a selector.
"
Class {
	#name : #SelectorNode,
	#superclass : #LeafNode,
	#category : #Compiler-ParseNodes
}

{ #category : #visiting }
SelectorNode >> accept: aVisitor [
	^aVisitor visitSelectorNode: self
]

{ #category : #'code generation' }
SelectorNode >> emitCode: stack args: nArgs encoder: encoder [

	self emitCode: stack
		args: nArgs
		encoder: encoder
		super: false
]

{ #category : #'code generation' }
SelectorNode >> emitCode: stack args: nArgs encoder: encoder super: supered [
	stack pop: nArgs.
	^supered
		ifTrue:
			[encoder genSendSuper: index numArgs: nArgs]
		ifFalse:
			[encoder
				genSend: (code < Send ifTrue: [code negated] ifFalse: [index])
				numArgs: nArgs]
]

{ #category : #'code generation' }
SelectorNode >> emitCodeForEffect: stack encoder: encoder [

	self shouldNotImplement
]

{ #category : #'code generation' }
SelectorNode >> emitCodeForValue: stack encoder: encoder [

	self shouldNotImplement
]

{ #category : #testing }
SelectorNode >> isForFFICall [
	^key includesSubstring: '()/'
]

{ #category : #printing }
SelectorNode >> key: aSelector [
	"This is for printing of FFI selectors."
	key := aSelector
]

{ #category : #initialize-release }
SelectorNode >> name: literal key: object index: i type: type [
	"For compatibility with Encoder>>name:key:class:type:set:"
	^self key: object index: i type: type
]

{ #category : #printing }
SelectorNode >> printAsFFICallWithArguments: aSequence on: aStream indent: level [
	aStream nextPutAll: (key copyUpTo: $)).
	aSequence
		do: [:arg| arg printOn: aStream indent: level]
		separatedBy: [aStream nextPutAll: ', '].
	aStream nextPut: $)
]

{ #category : #printing }
SelectorNode >> printOn: aStream indent: level [ 
	aStream nextPutAll: (key == nil
							ifTrue: ['<key==nil>']
							ifFalse: [key])
]

{ #category : #'code generation' }
SelectorNode >> reserve: encoder [ 
	"If this is a yet unused literal of type -code, reserve it."

	code < 0 ifTrue: [code := self code: (index := encoder sharableLitIndex: key) type: 0 - code]
]

{ #category : #'code generation' }
SelectorNode >> sizeCode: encoder args: nArgs super: supered [
	self reserve: encoder.
	^supered
		ifTrue:
			[code < Send "i.e. its a special selector" ifTrue:
				[code := self code: (index := encoder sharableLitIndex: key) type: 5].
			 encoder sizeSendSuper: index numArgs: nArgs]
		ifFalse:
			[self flag: #yuck. "special selector sends cause this problem"
			 encoder
				sizeSend: (code < Send ifTrue: [code negated] ifFalse: [index])
				numArgs: nArgs]
]

{ #category : #'code generation' }
SelectorNode >> sizeCodeForEffect: encoder [

	self shouldNotImplement
]

{ #category : #'code generation' }
SelectorNode >> sizeCodeForValue: encoder [

	self shouldNotImplement
]
