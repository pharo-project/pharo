"
I represent the Slot initialization part of a slot definition. 
I'm a transformation from the AST.

For example for this expression 'first' => (LazyClassVariable default: 5), I represent 
the default: 5 part. 

Slot initialization node works for the three slot forms below. 
To make sure that all slot definition are polymorphic, all slots have a initializationNode (some acting as null node).

Object subclass: #MyClass
		slots: {
		'first' => LazyClassVariable default: 5. 
		'second' => InstanceVariableSlot.
		'instVar3'
		}
		classVariableNames: ''
		package: #MyPackage'

'first' => LazyClassVariable default: x + 3

In this case the argument parts is a RBMessageNode.


		
	
"
Class {
	#name : 'RBSlotInitializationNode',
	#superclass : 'ASTNode',
	#instVars : [
		'selectorParts',
		'argumentParts'
	],
	#category : 'ClassParser-Model',
	#package : 'ClassParser',
	#tag : 'Model'
}

{ #category : 'instance creation' }
RBSlotInitializationNode class >> selectorParts: selectors argumentParts: aCollection [
	^ self new
		selectorParts: selectors;
		argumentParts: aCollection;
		yourself
]

{ #category : 'comparing' }
RBSlotInitializationNode >> = anObject [
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
 	(self selectorParts = anObject selectorParts) ifFalse: [^false].
	^ self argumentParts = anObject argumentParts
]

{ #category : 'visiting' }
RBSlotInitializationNode >> acceptVisitor: aVisitor [
	"Continue the navigation only on non null messages."
	^ self isNull ifFalse: [ aVisitor visitSlotInitializationNode: self ]
]

{ #category : 'accessing' }
RBSlotInitializationNode >> argumentParts [
	^ argumentParts
]

{ #category : 'accessing' }
RBSlotInitializationNode >> argumentParts: aCollection [
	argumentParts := aCollection
]

{ #category : 'comparing' }
RBSlotInitializationNode >> hash [
	^ self hashForCollection: self arguments
]

{ #category : 'initialization' }
RBSlotInitializationNode >> initialize [

	super initialize.
	selectorParts := #().
	argumentParts := #()
]

{ #category : 'testing' }
RBSlotInitializationNode >> isNull [
	^ selectorParts isEmpty
]

{ #category : 'printing' }
RBSlotInitializationNode >> printOn: aStream [
	"We make sure that the result is empty to be used like a bad null pattern now."

	self isNull
		ifFalse: [
				| size |
				size := selectorParts size.
				selectorParts withIndexDo: [ :each :i |
							aStream
								nextPutAll:  each asString  ;
								<< ' ';
								print: (argumentParts at: i) .
							i = size ifFalse: [ aStream	<< ' ']]]
]

{ #category : 'accessing' }
RBSlotInitializationNode >> selectorParts [
	^ selectorParts
]

{ #category : 'accessing' }
RBSlotInitializationNode >> selectorParts: aCollection [
	selectorParts := aCollection
]

{ #category : 'accessing' }
RBSlotInitializationNode >> selectorString [
	^ String streamContents: [ :s |
			selectorParts do: [ :e | s <<e ] ]
]
