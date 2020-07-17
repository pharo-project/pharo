Extension { #name : #StSindarinBytecodeContextInspectorModel }

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
