"
I represent a variable shared by instances of my class. (class variable)
"
Class {
	#name : #CDSharedVariableNode,
	#superclass : #CDSlotNode,
	#category : #'ClassParser-Model'
}

{ #category : #'instance-creation' }
CDSharedVariableNode class >> slot: aSlot node: aNode [ 
	
	^ self new
		slot: aSlot;
		node: aNode;
		yourself
]

{ #category : #transforming }
CDSharedVariableNode >> asClassVariable [
	^ ClassVariable named: self name.
]
