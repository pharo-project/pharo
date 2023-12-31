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
	^{ASTMessageNode . ASTMethodNode}
]

{ #category : 'plugin interface' }
RFSenderReification class >> key [
	^#sender
]

{ #category : 'generate' }
RFSenderReification >> genForRBMessageNode [
	^ASTVariableNode selfNode
]

{ #category : 'generate' }
RFSenderReification >> genForRBMethodNode [
	^ASTMessageNode
		receiver: (ASTMessageNode
			receiver: ASTVariableNode thisContextNode
			selector: #sender)
		selector: #receiver
]
