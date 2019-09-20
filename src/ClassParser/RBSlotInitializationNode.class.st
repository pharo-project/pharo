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

argumentParts holds a collection of strings since we can have 

'first' => LazyClassVariable default: x + 3

In this case the argument part is #('x + 3')


		
	
"
Class {
	#name : #RBSlotInitializationNode,
	#superclass : #RBNode,
	#instVars : [
		'selectorParts',
		'argumentParts'
	],
	#category : #'ClassParser-Model'
}

{ #category : #'instance creation' }
RBSlotInitializationNode class >> selectorParts: selectors argumentParts: aCollection [
	^ self new
		selectorParts: selectors;
		argumentParts: aCollection;
		yourself
]

{ #category : #comparing }
RBSlotInitializationNode >> = anObject [ 
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
 	(self selectorParts = anObject selectorParts) ifFalse: [^false].
	^ self argumentParts = anObject argumentParts
]

{ #category : #visiting }
RBSlotInitializationNode >> acceptVisitor: aVisitor [
	"Continue the navigation only on non null messages."
	^ self isNull ifFalse: [ aVisitor visitSlotInitializationNode: self ]
]

{ #category : #accessing }
RBSlotInitializationNode >> argumentParts [
	^ argumentParts 
]

{ #category : #accessing }
RBSlotInitializationNode >> argumentParts: aCollection [ 
	argumentParts := aCollection
]

{ #category : #comparing }
RBSlotInitializationNode >> hash [
	^ self hashForCollection: self arguments
]

{ #category : #initialization }
RBSlotInitializationNode >> initialize [ 
	
	super initialize.
	selectorParts := #().
	argumentParts := #()
]

{ #category : #testing }
RBSlotInitializationNode >> isNull [ 
	^ selectorParts isEmpty
]

{ #category : #printing }
RBSlotInitializationNode >> printOn: aStream [
	"We make sure that the result is empty to be used like a bad null pattern now."
	
	self isNull 
		ifFalse: [ 
				| size |
				size := selectorParts size. 
				selectorParts withIndexDo: [ :each :i |
							aStream 
								<< each asString ;
								<< ' ';
								<< (argumentParts at: i) printString.
							i = size ifFalse: [ aStream	<< ' ']]]
]

{ #category : #accessing }
RBSlotInitializationNode >> selectorParts [
	^ selectorParts 
]

{ #category : #accessing }
RBSlotInitializationNode >> selectorParts: aCollection [ 
	selectorParts := aCollection
]

{ #category : #accessing }
RBSlotInitializationNode >> selectorString [
	^ String streamContents: [ :s |  
			selectorParts do: [ :e | s <<e ] ] 
]
