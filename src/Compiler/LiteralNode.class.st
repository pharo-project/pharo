"
I am a parse tree leaf representing a literal string or number.
"
Class {
	#name : #LiteralNode,
	#superclass : #LeafNode,
	#category : #Compiler-ParseNodes
}

{ #category : #visiting }
LiteralNode >> accept: aVisitor [
	^aVisitor visitLiteralNode: self
]

{ #category : #'code generation' }
LiteralNode >> emitCodeForValue: stack encoder: encoder [
	stack push: 1.
	(encoder
		if: code
		isSpecialLiteralForPush:
			[:specialLiteral|
			 encoder genPushSpecialLiteral: specialLiteral])
		ifFalse:
			[encoder genPushLiteral: index]
]

{ #category : #testing }
LiteralNode >> isConstantNumber [
	^ key isNumber
]

{ #category : #testing }
LiteralNode >> isLiteralNode [

	^ true
]

{ #category : #testing }
LiteralNode >> isSpecialConstant [
	^ code between: LdTrue and: LdMinus1+3
]

{ #category : #testing }
LiteralNode >> literalValue [

	^key
]

{ #category : #initialize-release }
LiteralNode >> name: literal key: object index: i type: type [
	"For compatibility with Encoder>>name:key:class:type:set:"
	^self key: object index: i type: type
]

{ #category : #printing }
LiteralNode >> printOn: aStream indent: level [

	key isVariableBinding
		ifTrue:
			[key key 
				ifNil:
					[aStream nextPutAll: '###'; nextPutAll: key value soleInstance name]
				ifNotNil:
					[aStream nextPutAll: '##'; nextPutAll: key key]]
		ifFalse:
			[key storeOn: aStream]
]

{ #category : #'code generation' }
LiteralNode >> sizeCodeForValue: encoder [
	self reserve: encoder.
	(encoder
		if: code
		isSpecialLiteralForPush:
			[:specialLiteral|
			 ^encoder sizePushSpecialLiteral: specialLiteral])
		ifFalse:
			[^encoder sizePushLiteral: index]
]
