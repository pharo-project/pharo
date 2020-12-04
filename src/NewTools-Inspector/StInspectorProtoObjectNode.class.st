"
I am a variable node for representing the slots of objects that subclass ProtoObject directly and not Object. My main responsability is to extract the value of a slot using thisContext and not a direct message send to the object. 
I use the name of a slot as a key. 
I extract the value from the host object every time #rawValue is called.
"
Class {
	#name : #StInspectorProtoObjectNode,
	#superclass : #StInspectorNode,
	#instVars : [
		'index',
		'slotName'
	],
	#category : #'NewTools-Inspector-Model'
}

{ #category : #'instance creation' }
StInspectorProtoObjectNode class >> hostObject: anObject index: index slotName: aSlotName [
	^ (self hostObject: anObject)
		index: index slotName: aSlotName;
		yourself
]

{ #category : #accessing }
StInspectorProtoObjectNode >> index: anIndex slotName: aSlotName [
	index := anIndex.
	slotName := aSlotName.
]

{ #category : #accessing }
StInspectorProtoObjectNode >> key [
	^ slotName
]

{ #category : #accessing }
StInspectorProtoObjectNode >> rawValue [ 

	^ thisContext object: self hostObject instVarAt: index 
]

{ #category : #accessing }
StInspectorProtoObjectNode >> variableTag [
	^ 'attribute'
]
