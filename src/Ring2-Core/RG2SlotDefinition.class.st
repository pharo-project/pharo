Class {
	#name : #RG2SlotDefinition,
	#superclass : #RG2ElementDefinition,
	#category : #'Ring2-Core-Kernel'
}

{ #category : #bootstrap }
RG2SlotDefinition >> accept: anInterpreter assign: aValue inNode: aVariableNode [
	self error: #TBD
]

{ #category : #'managing container' }
RG2SlotDefinition >> addoptToParentStub [

	super addoptToParentStub.
	self parent addSlot: self.
]

{ #category : #'managing container' }
RG2SlotDefinition >> defaultParentStub [

	^ self defaultFixedLayoutStub
]

{ #category : #'testing types' }
RG2SlotDefinition >> isSlot [

	^ true
]

{ #category : #testing }
RG2SlotDefinition >> isSpecial [

	^ true
]

{ #category : #'as yet unclassified' }
RG2SlotDefinition >> name: aString [

	self parent parent announceDefinitionChangeDuring: [ 
		super name: aString ].
]

{ #category : #resolving }
RG2SlotDefinition >> resolveBehaviorInformation [

	self isBehaviorInformationResolved ifTrue: [ ^ self ].
	super resolveBehaviorInformation.
	
	self resolveName.
]
