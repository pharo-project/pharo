"
I am a parse tree leaf representing an instance variable.
"
Class {
	#name : #InstanceVariableNode,
	#superclass : #VariableNode,
	#category : 'Compiler-ParseNodes'
}

{ #category : #visiting }
InstanceVariableNode >> accept: aVisitor [
	^aVisitor visitInstanceVariableNode: self
]

{ #category : #'code generation' }
InstanceVariableNode >> emitCodeForStore: stack encoder: encoder [
	encoder genStoreInstVar: index
]

{ #category : #'code generation' }
InstanceVariableNode >> emitCodeForStorePop: stack encoder: encoder [
	encoder genStorePopInstVar: index.
	stack pop: 1
]

{ #category : #'code generation' }
InstanceVariableNode >> emitCodeForValue: stack encoder: encoder [
	stack push: 1.
	^encoder genPushInstVar: index
]

{ #category : #initialize-release }
InstanceVariableNode >> name: varName index: varIndex [
	^self name: varName index: varIndex-1 type: LdInstType
]

{ #category : #'code generation' }
InstanceVariableNode >> sizeCodeForStore: encoder [
	^encoder sizeStoreInstVar: index
]

{ #category : #'code generation' }
InstanceVariableNode >> sizeCodeForStorePop: encoder [
	^encoder sizeStorePopInstVar: index
]

{ #category : #'code generation' }
InstanceVariableNode >> sizeCodeForValue: encoder [
	^encoder sizePushInstVar: index
]
