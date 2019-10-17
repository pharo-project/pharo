"
I'm a little visitor. I take the specific node of the string that has to be completed and based on that node I give the list of completion results.
"
Class {
	#name : #CompletionProducerVisitor,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'currentClass'
	],
	#classInstVars : [
		'aCollection'
	],
	#category : #'NECompletion-New'
}

{ #category : #accessing }
CompletionProducerVisitor >> completionContext: aClass [ 
	currentClass := aClass.
]

{ #category : #completion }
CompletionProducerVisitor >> completionListForNode: aRBNode [

	^ aRBNode acceptVisitor: self
]

{ #category : #utilities }
CompletionProducerVisitor >> select: aCollection beginningWith: aString [
	^ aCollection select: [ :each | each beginsWith: aString asString ]
]

{ #category : #visiting }
CompletionProducerVisitor >> visitArrayNode: aRBArrayNode [
	
	^ #()
]

{ #category : #visiting }
CompletionProducerVisitor >> visitAssignmentNode: aRBAssignmentNode [
	
	^ #()
]

{ #category : #visiting }
CompletionProducerVisitor >> visitBlockNode: aRBBlockNode [
	
	^ #()
]

{ #category : #visiting }
CompletionProducerVisitor >> visitLiteralArrayNode: aRBLiteralArrayNode [
	
	^ #()
]

{ #category : #visiting }
CompletionProducerVisitor >> visitLiteralNode: aRBLiteralValueNode [
	| value |
	
	value := aRBLiteralValueNode value.
	value isSymbol ifFalse: [ ^ #() ].
	"return all symbols that start with value"
	^Symbol allSymbols select: [ :each | (each beginsWith: value) and: [each isLiteralSymbol]]
]

{ #category : #visiting }
CompletionProducerVisitor >> visitMessageNode:  aRBMessageNode [
	"uses the TypingVisitor and the idea of double dispatch to avoid multiple if statements"
	| receiver | 
	receiver := aRBMessageNode receiver.
	
	(receiver hasProperty: #type)
		ifTrue: [ ^ self select: (receiver propertyAt: #type) allSelectors beginningWith: aRBMessageNode selector ] .
	^ self select: Symbol selectorTable beginningWith: aRBMessageNode selector
]

{ #category : #visiting }
CompletionProducerVisitor >> visitMethodNode: aRBMethodNode [
	^ self select: Symbol selectorTable beginningWith: aRBMethodNode selector
]

{ #category : #visiting }
CompletionProducerVisitor >> visitParseErrorNode: aRBLiteralValueNode [
	
	^ #()
]

{ #category : #visiting }
CompletionProducerVisitor >> visitPragmaNode: aPragmaNode [
	^ self select: Symbol allSymbols beginningWith: aPragmaNode selector
]

{ #category : #visiting }
CompletionProducerVisitor >> visitReturnNode: aNode [
	
	^ #()
]

{ #category : #visiting }
CompletionProducerVisitor >> visitSequenceNode: aRBSequenceNode [ 
	^ #()
]

{ #category : #visiting }
CompletionProducerVisitor >> visitSlotInitializationNode: aSlotInitializationNode [
	^ #()
]

{ #category : #visiting }
CompletionProducerVisitor >> visitThisContextNode: aThisContextNode [
	^ #()
]

{ #category : #visiting }
CompletionProducerVisitor >> visitVariableNode: aRBVariableNode [
	| lookupClass |
	lookupClass := currentClass ifNil: [ UndefinedObject ].
	aRBVariableNode isDefinition ifTrue: [ ^ (self select: Symbol allSymbols beginningWith: aRBVariableNode name) select: [ :each | each numArgs = 0 ] ].
   aRBVariableNode isArgument ifTrue: [ ^ (self select: Symbol allSymbols beginningWith: aRBVariableNode name) select: [ :each | each numArgs = 0 ] ].
	"using a stream to store results should be better"
	^	(self select: Smalltalk globals keys beginningWith: aRBVariableNode name), 
	 	(self select: (lookupClass allSlots collect: [ :each | each name ]) beginningWith: aRBVariableNode name),
		(self select: aRBVariableNode methodNode temporaryNames beginningWith: aRBVariableNode name),
		(self select: aRBVariableNode methodNode argumentNames beginningWith: aRBVariableNode name),
		(self select: lookupClass allClassVarNames beginningWith: aRBVariableNode name),
		(self select: (lookupClass allSharedPools flatCollect: [ :each | each classVarNames]) beginningWith: aRBVariableNode name)

]
