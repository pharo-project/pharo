"
I represent the Slot initialization part of a slot definition. 
I'm not super well integrated in the language AST because I'm a transformation from the AST.
For example for this expression 'first' =>  LazyClassVariable default: 5, I represent 
the default: 5 part. 
Still I support visiting protocol in case. 

Object subclass: #MyClass
		slots: {
		'first' => LazyClassVariable default: 5. 
		'second' => InstanceVariableSlot.
		'instVar3'
		}
		classVariableNames: ''
		package: #MyPackage'

In addition, to make sure that all slot definition are polymorphic 

start and end are not managed for the moment at my level. 


		
	
"
Class {
	#name : #RBSlotInitializationNode,
	#superclass : #RBNode,
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
RBSlotInitializationNode >> acceptVisitor: aVisitor [
	"Continue the navigation only on non null messages."
	^ self isNull ifFalse: [ aVisitor visitSlotInitializationNode: self ]
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

{ #category : #initialization }
RBSlotInitializationNode >> initialize [ 
	
	super initialize.
	selector := #''.
	argument := ''
]

{ #category : #testing }
RBSlotInitializationNode >> isNull [ 
	^ selector = #''
]

{ #category : #printing }
RBSlotInitializationNode >> printOn: aStream [
	"We make sure that the result is empty to be used like a bad null pattern now."
	
	self isNull 
		ifFalse: [  
			aStream 
				nextPutAll: selector asString; 
	 			space;
				nextPutAll: argument asString ]
]

{ #category : #accessing }
RBSlotInitializationNode >> selector [
	^ selector
]

{ #category : #accessing }
RBSlotInitializationNode >> selector: aSymbol [
	selector := aSymbol
]
