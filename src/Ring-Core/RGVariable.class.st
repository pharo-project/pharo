"
A variable definition
"
Class {
	#name : #RGVariable,
	#superclass : #RGElement,
	#category : #'Ring-Core-Kernel'
}

{ #category : #visiting }
RGVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitVariableNode: aNode
]

{ #category : #'read/write usage' }
RGVariable >> analyzeRead: aVariableNode by: aSemanticAnalyzer [
]

{ #category : #'read/write usage' }
RGVariable >> analyzeWrite: aVariableNode by: aSemanticAnalyzer [
]

{ #category : #accessing }
RGVariable >> definitionString [
	"non special globals are defined by the symbol"
	^self needsFullDefinition
		ifTrue: [ String streamContents: [ :aStream |
			aStream nextPutAll: '#'; nextPutAll: self name]]
		ifFalse: [self name printString]

]

{ #category : #testing }
RGVariable >> isArgumentVariable [
	^false
]

{ #category : #testing }
RGVariable >> isClassInstanceVariable [

	^false
]

{ #category : #testing }
RGVariable >> isClassVariable [

	^false
]

{ #category : #testing }
RGVariable >> isInstanceVariable [
	"check if the var is an instance variable (a Slot)"
	^false
]

{ #category : #testing }
RGVariable >> isLocalVariable [
	^false
]

{ #category : #testing }
RGVariable >> isPoolVariable [

	^false
]

{ #category : #testing }
RGVariable >> isReservedVariable [
	"Ring2 does not model self, super, thisContext"
	^false
]

{ #category : #testing }
RGVariable >> isSpecial [
	^ self
		deprecated: 'Please use #needsFullDefinition instead '
		on: '03/11/2020'
		in: #Pharo8
		transformWith: '`@receiver isSpecial' 
						-> '`@receiver needsFullDefinition'.

]

{ #category : #testing }
RGVariable >> isTempVariable [
	^false
]

{ #category : #testing }
RGVariable >> isUninitialized [
	^false
]

{ #category : #testing }
RGVariable >> isVariable [
	^true
]

{ #category : #generated }
RGVariable >> isVariableDefinition [

	self
		deprecated: 'Use #isVariable instead'
		transformWith: '`@rec isVariableDefinition' -> '`@rec isVariable'.
	^ self isVariable
]

{ #category : #testing }
RGVariable >> isWorkspaceVariable [
	^ false
]

{ #category : #testing }
RGVariable >> isWritable [
	"Ring2 variables are all writable, e.g. it does not model Arguments"
	^true
]

{ #category : #accessing }
RGVariable >> name: aString [

	self parent announceDefinitionChangeDuring: [ 
		super name: aString ].

]

{ #category : #testing }
RGVariable >> needsFullDefinition [
	"all but InstanceVariableSlot and ClassVariable need to print the full definition"
	^true
]
