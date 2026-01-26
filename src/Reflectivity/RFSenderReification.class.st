"
I reify the sender for message sends and methods
"
Class {
	#name : 'RFSenderReification',
	#superclass : 'RFReification',
	#category : 'Reflectivity-Reifications',
	#package : 'Reflectivity',
	#tag : 'Reifications'
}

{ #category : 'plugin interface' }
RFSenderReification class >> entities [
	^{OCMessageNode . OCMethodNode}
]

{ #category : 'plugin interface' }
RFSenderReification class >> key [
	^#sender
]

{ #category : 'generate' }
RFSenderReification >> genForASTMessageNode [
	^OCVariableNode selfNode
]

{ #category : 'generate' }
RFSenderReification >> genForASTMethodNode [
	^OCMessageNode
		receiver: (OCMessageNode
			receiver: OCVariableNode thisContextNode
			selector: #sender)
		selector: #receiver
]
