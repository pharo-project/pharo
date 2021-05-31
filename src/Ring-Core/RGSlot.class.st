Class {
	#name : #RGSlot,
	#superclass : #RGVariable,
	#category : #'Ring-Core-Kernel'
}

{ #category : #bootstrap }
RGSlot >> accept: anInterpreter assign: aValue inNode: aVariableNode [
	self error: #TBD
]

{ #category : #visiting }
RGSlot >> acceptVisitor: aProgramNodeVisitor node: aNode [ 
	^ aProgramNodeVisitor visitInstanceVariableNode: aNode
]

{ #category : #'managing container' }
RGSlot >> addoptToParentStub [

	super addoptToParentStub.
	self parent addSlot: self.
]

{ #category : #'managing container' }
RGSlot >> defaultParentStub [

	^ self defaultFixedLayoutStub
]

{ #category : #accessing }
RGSlot >> definitionString [

	^ String streamContents: [ :aStream |
		aStream nextPutAll: '#'; nextPutAll: self name]
]

{ #category : #testing }
RGSlot >> isInstanceVariable [
	^ true
]

{ #category : #'testing - types' }
RGSlot >> isSlot [

	^ true
]

{ #category : #generated }
RGSlot >> isSpecial [

	self
		deprecated: 'Use #needsFullDefinition instead'
		transformWith: '`@rec isSpecial' -> '`@rec needsFullDefinition'.
	^ self needsFullDefinition
]

{ #category : #accessing }
RGSlot >> name: aString [

	self parent parent announceDefinitionChangeDuring: [ 
		super name: aString ].
]
