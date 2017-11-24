"
I am a variable node for representing the slots of an object. I used the name of a slot as a key. I extract the value from the host object every time #rawValue is called.
"
Class {
	#name : #GTInspectorSlotNode,
	#superclass : #GTInspectorVariableNode,
	#instVars : [
		'slot'
	],
	#category : #'GT-Inspector'
}

{ #category : #'instance creation' }
GTInspectorSlotNode class >> hostObject: anObject slot: aSlot [
	^ (self hostObject: anObject)
		slot: aSlot;
		yourself
]

{ #category : #accessing }
GTInspectorSlotNode >> browsingActionsForNode [

	^ {
			GLMGenericAction new 
				action: [ self systemNavigation browseAllAccessesTo: self label from: self hostObject class ]; 
				title: 'List methods using this variable'.
			GLMGenericAction new 
				action: [ self systemNavigation browseAllStoresInto: self label from: self hostObject class ]; 
				title: 'List methods storing into this variable'.		
		}
]

{ #category : #accessing }
GTInspectorSlotNode >> errorMessage [
	^ 'error obtaining attribute value'
]

{ #category : #accessing }
GTInspectorSlotNode >> inspectorActionsForNode [
	"Return a collection of available inspector actions associated with a variable node"
	<return: #Collection of: #GLMAction>

	^ self browsingActionsForNode
]

{ #category : #accessing }
GTInspectorSlotNode >> key [
	^ self slot name
]

{ #category : #accessing }
GTInspectorSlotNode >> rawValue [
	^ self hostObject readSlot: slot
]

{ #category : #actions }
GTInspectorSlotNode >> save: anObject [
	self hostObject writeSlot: self slot value: anObject
]

{ #category : #accessing }
GTInspectorSlotNode >> slot [
	^ slot
]

{ #category : #accessing }
GTInspectorSlotNode >> slot: aSlot [

	self
		assert: [ aSlot isNotNil ]
		description: [ 'Slot can not be nil' ].

	slot := aSlot.
]

{ #category : #accessing }
GTInspectorSlotNode >> variableTag [
	^ 'attribute'
]
