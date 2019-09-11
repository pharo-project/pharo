"
I'm a Slot that represents one side of a relationship. If this side of the relationship is updated I take care of keeping the other side up to date.

I'm abstract, use  the ToOne or ToMany  subclass depending on the cardinality.

Instance Variables
	inverseName:		<String>
	inverseSlot:		<RelationSlot>
	targetClass:		<Class> or <Symbol>

inverseName
	- the name  of the slot of the other side of the relationship

inverseSlot
	- the slot of the other side of the relationship

targetClass
	- the class or the name of the class this slot refers to

"
Class {
	#name : #RelationSlot,
	#superclass : #InstanceVariableSlot,
	#instVars : [
		'targetClass',
		'inverseName',
		'inverseSlot'
	],
	#category : #'Slot-Examples-Associations'
}

{ #category : #'instance creation' }
RelationSlot class >> named: aSymbol inverse: anInverseSymbol inClass: aTargetClassOrSymbol [ 

	^(super named: aSymbol) inverse: anInverseSymbol inClass: aTargetClassOrSymbol 
]

{ #category : #comparing }
RelationSlot >> = anObject [ 

	^super = anObject and: [ 
		self targetClassName = anObject targetClassName and: [ 
			inverseName = anObject inverseName ] ]

	
]

{ #category : #internal }
RelationSlot >> addAssociationFrom: ownerObject to: otherObject [
	"A new reference from <ownerObject> to <otherObject> is created. Here we update the other 
	side of the association. If the other side is a ToOne association this means that an other
	association may have to be removed first."

	self inverseSlot isToOneSlot
		ifTrue: [ 
			| oldObject |
			oldObject := inverseSlot read: otherObject.
			oldObject notNil
				ifTrue: [ inverseSlot removeAssociationFrom: otherObject to: oldObject ].
			inverseSlot writeInverse: ownerObject to: otherObject ]
		ifFalse: [ 
			(inverseSlot read: otherObject) inverseAdd: ownerObject ]
]

{ #category : #internal }
RelationSlot >> checkValue: aValue [

	(aValue isKindOf: self targetClass)
		ifFalse: [ self error: 'Invalid value' ]
]

{ #category : #'code generation' }
RelationSlot >> emitStore: aMethodBuilder [
	"copy of #emitStore from Slot as my superclass has overriden it"
	| tempName |
	tempName := '0slotTempForStackManipulation'.
	aMethodBuilder
		addTemp: tempName;
		storeTemp: tempName;
		popTop;
		pushReceiver;
		pushLiteralVariable: (AdditionalBinding key: #slot value: self);
		pushTemp: tempName;
		send: #writeSlot:value:
]

{ #category : #testing }
RelationSlot >> hasInverse [

	^inverseName notNil
]

{ #category : #comparing }
RelationSlot >> hasSameDefinitionAs: otherSlot [
	"need to implement as superclass implements it"
	^ (super hasSameDefinitionAs: otherSlot) 
		and: [ self targetClassName = otherSlot targetClassName 
		and: [ inverseName = otherSlot inverseName ] ]
]

{ #category : #comparing }
RelationSlot >> hash [
	^ ((self species hash bitXor: self name hash) 
			bitXor: (self index ifNil: [ 0 ])) 
				bitXor: self targetClassName hash
]

{ #category : #initialization }
RelationSlot >> inClass: aTargetClassOrSymbol [

	targetClass := aTargetClassOrSymbol
]

{ #category : #initialization }
RelationSlot >> inverse: anInverseSymbol inClass: aTargetClassOrSymbol [
	self inClass: aTargetClassOrSymbol.
	inverseName := anInverseSymbol
]

{ #category : #accessing }
RelationSlot >> inverseName [

	^inverseName
]

{ #category : #accessing }
RelationSlot >> inverseSlot [

	^inverseSlot ifNil: [ self linkUp. inverseSlot ]
]

{ #category : #testing }
RelationSlot >> isToOneSlot [

	^false
]

{ #category : #initialization }
RelationSlot >> linkUp [

	inverseSlot := self targetClass slotNamed: inverseName.
	(inverseSlot isKindOf: RelationSlot)
		ifFalse: [ self error: 'Invalid association: ... ' ].
		
	inverseSlot inverseName = self name
		ifFalse: [ self error: 'Invalid association: inverse names do not match' ].
]

{ #category : #printing }
RelationSlot >> printOn: aStream [
	aStream 
		store: self name;
		nextPutAll: ' => ';
		nextPutAll: self class name.
	self hasInverse 
		ifTrue: [ 
			aStream
				nextPutAll: ' inverse: ';
				store: inverseName ].
	aStream
		nextPutAll: ' inClass: ';
		store: self targetClassName

]

{ #category : #internal }
RelationSlot >> removeAssociationFrom: ownerObject to: otherObject [
	"A reference from <ownerObject> to <otherObject> is removed. Here we update the other 
	side of the association. If the other side is a ToOne association set the value to nil,
	for ToMany associations remove <ownerObject> from the collection." 

	self inverseSlot isToOneSlot
		ifTrue: [ inverseSlot writeInverse: nil to: otherObject ]
		ifFalse: [ (inverseSlot read: otherObject) inverseRemove: ownerObject ]
]

{ #category : #accessing }
RelationSlot >> targetClass [

	targetClass isSymbol
		ifTrue: [ 
			targetClass := Smalltalk globals at: targetClass 
				ifAbsent: [ self error: 'Cannot find class ', targetClass printString ] ].
	
	^targetClass
]

{ #category : #accessing }
RelationSlot >> targetClassName [

	^targetClass isSymbol
		ifTrue: [ targetClass ]
		ifFalse: [ targetClass name ]
]
