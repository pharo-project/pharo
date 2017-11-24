"
I am a variable node for representing the slots of objects that subclass ProtoObject directly and not Object. My main responsability is to extract the value of a slot using thisContext and not a direct message send to the object.  I used the name of a slot as a key. I extract the value from the host object every time #rawValue is called.
"
Class {
	#name : #GTInspectorProtoObjectNode,
	#superclass : #GTInspectorVariableNode,
	#instVars : [
		'index',
		'slotName'
	],
	#category : #'GT-Inspector'
}

{ #category : #'instance creation' }
GTInspectorProtoObjectNode class >> hostObject: anObject index: index slotName: aSlotName [
	^ (self hostObject: anObject)
		index: index slotName: aSlotName;
		yourself
]

{ #category : #accessing }
GTInspectorProtoObjectNode >> index: anIndex slotName: aSlotName [
	index := anIndex.
	slotName := aSlotName.
]

{ #category : #accessing }
GTInspectorProtoObjectNode >> key [
	^ slotName
]

{ #category : #accessing }
GTInspectorProtoObjectNode >> rawValue [ 
	^ thisContext object: self hostObject instVarAt: index 
]

{ #category : #accessing }
GTInspectorProtoObjectNode >> variableTag [
	^ 'attribute'
]
