Class {
	#name : #StSindarinBytecodeContextInspectorModel,
	#superclass : #StInspectorModel,
	#category : #'NewTools-Sindarin-Tools'
}

{ #category : #accessing }
StSindarinBytecodeContextInspectorModel >> allInspectorNodes [
	| tempNodes slotNodes stackTopNode |
	tempNodes := self contextTempNodes.
	slotNodes := self contextReceiverSlotNodes.
	stackTopNode := self stacktopNode.
	^ OrderedCollection new
		add: stackTopNode;
		addAll: tempNodes;
		addAll: slotNodes;
		yourself
]

{ #category : #'*NewTools-Sindarin-Tools' }
StSindarinBytecodeContextInspectorModel >> contextReceiverSlotNodes [
	^ self inspectedObject receiver class allSlots
		collect: [ :each | 
			StBytecodeInspectorSlotNode
				hostObject: self inspectedObject receiver
				slot: each ]
]

{ #category : #'*NewTools-Sindarin-Tools' }
StSindarinBytecodeContextInspectorModel >> contextTempNodes [
	| tempNodes |
	tempNodes := StNodeCollector new tempNodesForContext: self inspectedObject.
	tempNodes
		do:
			[ :node | node tempName: (node tempIndex - 1) asString , ' [' , node tempName , ']' ].
	^ tempNodes
]

{ #category : #accessing }
StSindarinBytecodeContextInspectorModel >> stacktopNode [
	^ StInspectorStackTopNode hostObject: self inspectedObject
]
