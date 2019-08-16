"
I'm a little visitor. I take the specific node of the string that has to be completed and based on that node I give the list of completion results.
"
Class {
	#name : #CompletionProducer,
	#superclass : #Object,
	#instVars : [
		'currentClass'
	],
	#category : #'NECompletion-New'
}

{ #category : #accessing }
CompletionProducer >> completionContext: aClass [ 
	currentClass := aClass.
]

{ #category : #completion }
CompletionProducer >> completionListForNode: aRBNode [

	^ aRBNode acceptVisitor: self
]

{ #category : #visiting }
CompletionProducer >> methodNames [
	^ Symbol selectorTable
	
]

{ #category : #utilities }
CompletionProducer >> select: aCollection beginningWith: aString [
	"Set withAll: is needed to convert potential IdentitySets to regular Sets"
	^ Set withAll: (aCollection select: [ :each | each beginsWith: aString asString ])
]

{ #category : #visiting }
CompletionProducer >> visitArgumentNode: anArgumentNode [
	"Sent *each time* an argument node is found"
	^ self visitVariableNode: anArgumentNode
]

{ #category : #visiting }
CompletionProducer >> visitArrayNode: aRBArrayNode [
	
	^ #()
]

{ #category : #visiting }
CompletionProducer >> visitAssignmentNode: aRBAssignmentNode [
	
	^ #()
]

{ #category : #visiting }
CompletionProducer >> visitBlockNode: aRBBlockNode [
	
	^ #()
]

{ #category : #visiting }
CompletionProducer >> visitGlobalNode: aRBGlobalNode [ 
	^ self visitVariableNode: aRBGlobalNode 
]

{ #category : #visiting }
CompletionProducer >> visitInstanceVariableNode: aSelfNode [
	^ self visitVariableNode: aSelfNode
]

{ #category : #visiting }
CompletionProducer >> visitLiteralArrayNode: aRBLiteralArrayNode [
	
	^ #()
]

{ #category : #visiting }
CompletionProducer >> visitLiteralNode: aRBLiteralValueNode [
	| results |
	(aRBLiteralValueNode value isKindOf: Symbol) ifFalse: [ ^#() ].
	"return all symbols that start with value"
	results := OrderedCollection new.
	Symbol allSymbolTablesDo: [ :symbol |
		(symbol beginsWith: aRBLiteralValueNode value)
			ifTrue: [ results add: symbol ].
		results size >= 10 ifTrue: [ ^ results ]
	].
	^ results
]

{ #category : #visiting }
CompletionProducer >> visitLiteralValueNode: aRBLiteralValueNode [ 
	^ self visitLiteralNode: aRBLiteralValueNode
]

{ #category : #visiting }
CompletionProducer >> visitMessageNode:  aRBMessageNode [
	"uses the TypingVisitor and the idea of double dispatch to avoid multiple if statements"
	| receiver | 
	receiver := aRBMessageNode receiver.
	
	(receiver hasProperty: #type)
		ifTrue: [ ^ self select: (receiver propertyAt: #type) allSelectors beginningWith: aRBMessageNode selector ] .
	^ self select: Symbol selectorTable beginningWith: aRBMessageNode selector
]

{ #category : #visiting }
CompletionProducer >> visitMethodNode: aRBMethodNode [ 
		
	^(self select: self methodNames beginningWith: aRBMethodNode selector)
]

{ #category : #visiting }
CompletionProducer >> visitNode: aNode [ 
	^aNode acceptVisitor: self
]

{ #category : #visiting }
CompletionProducer >> visitParseErrorNode: aRBLiteralValueNode [
	
	^ #()
]

{ #category : #visiting }
CompletionProducer >> visitPragmaNode: aPragmaNode [
	^ self select: Symbol allSymbols beginningWith: aPragmaNode selector
]

{ #category : #visiting }
CompletionProducer >> visitReturnNode: aNode [
	
	^ #()
]

{ #category : #visiting }
CompletionProducer >> visitSelfNode: aRBSelfNode [ 
	^ self visitVariableNode: aRBSelfNode 
]

{ #category : #visiting }
CompletionProducer >> visitSequenceNode: aRBSequenceNode [ 
	^ #()
]

{ #category : #visiting }
CompletionProducer >> visitSuperNode: aSuperNode [
	^ self visitVariableNode: aSuperNode
]

{ #category : #visiting }
CompletionProducer >> visitTemporaryNode: aNode [ 
	"Sent *each time* a temporary node is found"
	^ self visitVariableNode: aNode
]

{ #category : #visiting }
CompletionProducer >> visitVariableNode: aRBVariableNode [
	| lookupClass |
	lookupClass := currentClass ifNil: [ UndefinedObject ].
	aRBVariableNode isDefinition ifTrue: [ ^ (self select: Symbol allSymbols beginningWith: aRBVariableNode name) select: [ :each | each numArgs = 0 ] ].
   aRBVariableNode isArgument ifTrue: [ ^ (self select: Symbol allSymbols beginningWith: aRBVariableNode name) select: [ :each | each numArgs = 0 ] ].
	"using a stream to store results should be better"
	^ (self select: Smalltalk globals keys beginningWith: aRBVariableNode name) , 
	  (self select: (lookupClass allSlots collect: [ :each | each name ]) beginningWith: aRBVariableNode name) ,
		(self select: aRBVariableNode methodNode temporaryNames beginningWith: aRBVariableNode name),
		(self select: aRBVariableNode methodNode argumentNames beginningWith: aRBVariableNode name),
		(self select: lookupClass allClassVarNames beginningWith: aRBVariableNode name),
		(self select: (lookupClass allSharedPools flatCollect: [ :each | each classVarNames]) beginningWith: aRBVariableNode name)
		",(self select: self methodNames beginningWith: aRBVariableNode name)"

]
