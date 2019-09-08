Class {
	#name : #RBSlotInitializationNode,
	#superclass : #RBValueNode,
	#instVars : [
		'selector',
		'argument'
	],
	#category : #'ClassParser-Model'
}

{ #category : #'instance creation' }
RBSlotInitializationNode class >> selector: aSelector argument: value [
	^ self new
		selector: aSelector;
		argument: value;
		yourself
]

{ #category : #comparing }
RBSlotInitializationNode >> = anObject [ 
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
 	(self selector = anObject selector) ifFalse: [^false].
	^ self argument = anObject argument
]

{ #category : #visiting }
RBSlotInitializationNode >> accept: aVisitor [

	^ aVisitor visitSlotInitializationNode: self
]

{ #category : #accessing }
RBSlotInitializationNode >> argument [
	^ argument
]

{ #category : #accessing }
RBSlotInitializationNode >> argument: anInteger [ 
	argument := anInteger
]

{ #category : #comparing }
RBSlotInitializationNode >> hash [
	^ self hashForCollection: self arguments
]

{ #category : #printing }
RBSlotInitializationNode >> printOn: aStream [

	aStream 
		<< selector asString ; 
		space; 
		print: argument 
]

{ #category : #accessing }
RBSlotInitializationNode >> selector [
	^ selector
]

{ #category : #accessing }
RBSlotInitializationNode >> selector: aSymbol [
	selector := aSymbol
]
