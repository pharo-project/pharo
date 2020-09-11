"
I reify the sender for message sends and methods
"
Class {
	#name : #RFSenderReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Reifications'
}

{ #category : #'plugin interface' }
RFSenderReification class >> entities [
	^{RBMessageNode . RBMethodNode}
]

{ #category : #'plugin interface' }
RFSenderReification class >> key [
	^#sender
]

{ #category : #generate }
RFSenderReification >> genForRBMessageNode [
	^RBVariableNode selfNode
]

{ #category : #generate }
RFSenderReification >> genForRBMethodNode [
	^RBMessageNode 
		receiver: (RBMessageNode 
			receiver: RBVariableNode thisContextNode
			selector: #sender)
		selector: #receiver.
		
]
