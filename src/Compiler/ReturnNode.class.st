"
I represent an expression of the form ^expr.
"
Class {
	#name : #ReturnNode,
	#superclass : #ParseNode,
	#instVars : [
		'expr'
	],
	#category : #'Compiler-ParseNodes'
}

{ #category : #visiting }
ReturnNode >> accept: aVisitor [
	^aVisitor visitReturnNode: self
]

{ #category : #'code generation (closures)' }
ReturnNode >> analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools [ "<Dictionary>"
	"Note we could do this:
		scopeBlock ~~ rootNode block ifTrue:
			[scopeBlock noteNonLocalReturn].
	 and pass up the flag in <BlockNode>>>analyseTempsWithin:rootNode:
	 which may be fast but will also give less information the debugger.
	 For now we consider clean blocks a premature optimization."
	self flag: 'consider clean blocks'.
	expr analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools
]

{ #category : #converting }
ReturnNode >> asReturnNode [
]

{ #category : #'code generation' }
ReturnNode >> code [

	^expr code
]

{ #category : #'code generation' }
ReturnNode >> emitCodeForReturn: stack encoder: encoder [

	expr emitCodeForReturn: stack encoder: encoder.
	pc := encoder methodStreamPosition
]

{ #category : #'code generation' }
ReturnNode >> emitCodeForValue: stack encoder: encoder [

	expr emitCodeForReturn: stack encoder: encoder.
	pc := encoder methodStreamPosition
]

{ #category : #printing }
ReturnNode >> expr [

	^ expr.

]

{ #category : #'initialize-release' }
ReturnNode >> expr: e [

	expr := e
]

{ #category : #'initialize-release' }
ReturnNode >> expr: e encoder: encoder sourceRange: range [

	expr := e.
	encoder noteSourceRange: range forNode: self
]

{ #category : #testing }
ReturnNode >> isReturn [
	^true
]

{ #category : #testing }
ReturnNode >> isReturnSelf [

	^expr == NodeSelf
]

{ #category : #testing }
ReturnNode >> isSpecialConstant [

	^expr isSpecialConstant
]

{ #category : #testing }
ReturnNode >> isVariableReference [

	^expr isVariableReference
]

{ #category : #printing }
ReturnNode >> printOn: aStream indent: level [

	aStream nextPutAll: '^ '. 
	expr printOn: aStream indent: level.
	expr printCommentOn: aStream indent: level
]

{ #category : #'code generation' }
ReturnNode >> sizeCodeForReturn: encoder [

	^expr sizeCodeForReturn: encoder
]

{ #category : #'code generation' }
ReturnNode >> sizeCodeForValue: encoder [

	^expr sizeCodeForReturn: encoder
]
