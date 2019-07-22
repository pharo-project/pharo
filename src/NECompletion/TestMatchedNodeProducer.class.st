"
I'm a little visitor. I take the specific node of the string that has to be completed and based on that node I give the list of completion results.
"
Class {
	#name : #TestMatchedNodeProducer,
	#superclass : #Object,
	#instVars : [
		'currentClass'
	],
	#category : #'NECompletion-New'
}

{ #category : #accessing }
TestMatchedNodeProducer >> completionContext: aClass [ 
	currentClass := aClass.
]

{ #category : #completion }
TestMatchedNodeProducer >> completionListForNode: aRBNode [

	^ aRBNode acceptVisitor: self
]

{ #category : #visiting }
TestMatchedNodeProducer >> methodNames [
	^ Symbol selectorTable
	
]

{ #category : #utilities }
TestMatchedNodeProducer >> select: aCollection beginningWith: aString [
	"Set withAll: is needed to convert potential IdentitySets to regular Sets"
	^ Set withAll: (aCollection select: [ :each | each beginsWith: aString asString ])
]

{ #category : #visiting }
TestMatchedNodeProducer >> visitArgumentNode: anArgumentNode [
	"Sent *each time* an argument node is found"
	^ self visitVariableNode: anArgumentNode
]

{ #category : #visiting }
TestMatchedNodeProducer >> visitGlobalNode: aRBGlobalNode [ 
	^ self visitVariableNode: aRBGlobalNode 
]

{ #category : #visiting }
TestMatchedNodeProducer >> visitLiteralNode: aRBLiteralValueNode [
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
TestMatchedNodeProducer >> visitLiteralValueNode: aRBLiteralValueNode [ 
	^ self visitLiteralNode: aRBLiteralValueNode
]

{ #category : #visiting }
TestMatchedNodeProducer >> visitMessageNode:  aRBMessageNode [
	"uses the TypingVisitor and the idea of double dispatch to avoid multiple if statements"
	| receiver | 
	receiver := aRBMessageNode receiver.
	
	(receiver hasProperty: #type)
		ifTrue: [ ^ self select: (receiver propertyAt: #type) allSelectors beginningWith: aRBMessageNode selector ] .
	^ self select: Symbol selectorTable beginningWith: aRBMessageNode selector
]

{ #category : #visiting }
TestMatchedNodeProducer >> visitMethodNode: aRBMethodNode [ 
	"there is a bug that we end up here when in temp definition, workaround turn it off for now"
	"this turns off completion of methods and temp defitions, see issue tracker"
	 ^#().
	"(self select: self methodNames beginningWith: aRBMethodNode selector)"
]

{ #category : #visiting }
TestMatchedNodeProducer >> visitNode: aNode [ 
	^aNode acceptVisitor: self
]

{ #category : #visiting }
TestMatchedNodeProducer >> visitParseErrorNode: aRBLiteralValueNode [
	
	^ #()
]

{ #category : #visiting }
TestMatchedNodeProducer >> visitPragmaNode: aPragmaNode [
	^ self select: Symbol allSymbols beginningWith: aPragmaNode selector
]

{ #category : #visiting }
TestMatchedNodeProducer >> visitSelfNode: aRBSelfNode [ 
	^ self visitVariableNode: aRBSelfNode 
]

{ #category : #visiting }
TestMatchedNodeProducer >> visitSequenceNode: aRBSequenceNode [ 
	^ #()
]

{ #category : #visiting }
TestMatchedNodeProducer >> visitSuperNode: aSuperNode [
	^ self visitVariableNode: aSuperNode
]

{ #category : #visiting }
TestMatchedNodeProducer >> visitTemporaryNode: aNode [ 
	"Sent *each time* a temporary node is found"
	^ self visitVariableNode: aNode
]

{ #category : #visiting }
TestMatchedNodeProducer >> visitVariableNode: aRBVariableNode [  
	aRBVariableNode isDefinition ifTrue: [ ^ (self select: Symbol allSymbols beginningWith: aRBVariableNode name) ].
   (aRBVariableNode isArgument ifTrue: [ ^ (self select: Symbol allSymbols beginningWith: aRBVariableNode name) ]).
	"using a stream to store results should be better"
	^ (self select: Smalltalk globals keys beginningWith: aRBVariableNode name) , 
	  (self select: (currentClass allSlots collect: [ :each | each name ]) beginningWith: aRBVariableNode name) ,
		(self select: aRBVariableNode methodNode temporaryNames beginningWith: aRBVariableNode name),
		(self select: aRBVariableNode methodNode argumentNames beginningWith: aRBVariableNode name),
		(self select: currentClass allClassVarNames beginningWith: aRBVariableNode name),
		(self select: (currentClass allSharedPools flatCollect: [ :each | each classVarNames]) beginningWith: aRBVariableNode name)
		",(self select: self methodNames beginningWith: aRBVariableNode name)" 
		"this is a bit too much, we limit to variables"

]
