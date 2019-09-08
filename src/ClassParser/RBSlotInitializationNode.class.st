Class {
	#name : #RBSlotInitializationNode,
	#superclass : #RBValueNode,
	#category : #'ClassParser-Model'
}

{ #category : #comparing }
RBSlotInitializationNode >> = anObject [ 
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
 	(self selector = anObject selector) ifFalse: [^false].
	self arguments
		with: anObject arguments
		do: [:first :second | first = second ifFalse: [^false]].
	^true
]

{ #category : #visiting }
RBSlotInitializationNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitSlotInitializationNode: self
]

{ #category : #comparing }
RBSlotInitializationNode >> hash [
	^ self hashForCollection: self arguments
]
