Class {
	#name : #RG2SlotDefinitionTest,
	#superclass : #RG2Test,
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
RG2SlotDefinitionTest >> testNewSlot [

	| slot | 
	slot  := RG2FixedLayoutDefinition unnamed.
	self assert: (slot isRing2Resolved).
	self assert: (slot hasUnresolved: #name).
	self assert: (slot isLayout).
	
]

{ #category : #tests }
RG2SlotDefinitionTest >> testSlotsImplicitEnvironment [

	| layout |
	
	layout := RG2InstanceVariableSlotDefinition unnamed.
	self assert: (layout hasUnresolved: #name).
	self assert: (layout parent isLayout).
	self checkImplicitSingleClassEnvironmentOf: layout parent parent.
	
	
	
	

]

{ #category : #tests }
RG2SlotDefinitionTest >> testUnknownSlot [

	| slot expression | 
	slot  := RG2UnknownSlotDefinition unnamed.
	self assert: (slot isRing2Resolved).
	self assert: (slot hasUnresolvedAll: #(name expression)).
	self assert: (slot isSlot).
	expression := 'ToManyRelationSlot inverse: #director inClass: #SlotExampleMovie'.
	slot expression: expression.
	self assert: slot expression equals: expression.
	
]
